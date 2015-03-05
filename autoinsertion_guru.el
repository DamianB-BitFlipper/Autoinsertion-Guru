
(require 'cl-lib)

;;
;; Version and About Information
;;


(defconst aig--about-name "Autoinsertion Guru"
  "The name displayed in the about information Autoinsertion Guru.")

(defconst aig--developers '("John Smith")
  "The list of strings for each developer activly involved in developing Autoinsertion Guru.")

(defconst aig--version "0.0.1" 
  "The version number for Autoinsertion Guru.")

;;
;; Version and About Information
;;

;;
;; User customizable variables
;;
(defgroup autoinsertion-guru nil
  "Automatically insert mini-snippets based on the context of the cursor."
  :group 'editing)

(defcustom aig-template-dirs (list "~/.emacs.d/aig-templates")
  "The directories where aig templates are search for in."
  :type '(repeat :args (directory) :tag "List of directories")
  :group 'autoinsertion-guru)

(defcustom aig-prompt-functions '(aig-ido-prompt
                                  aig-completing-prompt
                                  aig-first-choice-no-prompt)
  "Functions to prompt for choosing templates interactively.

These functions are called with the following arguments:

- PROMPT: A string to prompt the user

- CHOICES: a list of strings or objects."
  :type '(repeat function)
  :group 'autoinsertion-guru)
;;
;; User customizable variables
;;

;;
;; Some initial setup
;;

;;make a hash table test named aig--string= using string=
;; it is a bit more efficient than using equal
(define-hash-table-test 'aig--string= 'string= 'sxhash)

;;
;; Some initial setup
;;

;;
;; Global variables
;;

;;The structure housing the data for a single loaded template
;; hash - the hash table of a template
;; path - a string to the path of the template file, nil if there is no path
;; loaded-mode - a string representing the mode the current template is loaded in
(cl-defstruct aig-template hash path loaded-mode)

(defvar aig--hash-templates-by-mode (make-hash-table :test 'aig--string=)
  "A hash table that holds a list of hash tables of loaded 
templates for each respective mode identified by the key.

Each hash table element in each of the lists contained in this hash table is
guaranteed to have the following fields: name, hook, delim, expr
If on loading, those fields are missing, an error will be signaled.")

(defvar aig--list-context-hash-templates '() 
  "A list of hashes of templates that are enabled in the current context.
This variable is updated every time the context is updated.")

(defvar aig--guessed-modes nil
  "This variable is set as a buffer local variable. 
List of guessed modes supporting `aig-load-template-buffer'.")

(defvar aig--current-buffer-template-buffer nil
  "This variable is set as a buffer local variable. 
Boolean if the current buffer is a template buffer.")

(defvar aig--current-buffer-template nil
  "This variable is set as a buffer local variable. 
Holds the template structure representing an opened and loaded template buffer.
Note: this template structure is not guaranteed to be complete, it may be that
mode and path are valid, but hash still be nil. It is best to check each value before using them.")

(defconst aig--new-template-buffer-name "*new aig template*" 
  "The name of the buffer that is created when making a new template.")

(defconst aig--new-template-buffer-template
  "# name [template name]
# [This is a comment, only hook or hook-func have to be defined]
# [If both are defined, then both have to be satisfied to execute this template]
# hook [hook character]
# hook-func [hook function]
# delim [delim regex]
# expr [expr regex]
#--
\(lisp expression here\)
"
  "The template the is loaded into a new template buffer."
)

;;
;; Global variables
;;

;;
;; Load the Mode details
;;
(add-to-list 'load-path ".")
(require 'autoinsertion-guru-minor-mode "autoinsertion_guru_minor_mode.el")
;;
;; Load the Mode details
;;

;;
;; Utility Functions
;;

(defun aig--read-file (file-path)
  "Returns the file's contents as one large string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun aig--unescape-string (str)
  "Takes a string with escape chars and makes them into their equivalents processed.
ie: \n (2 chars) becomes newline (1 char)"
  ;;add quotes around the string so that read interprets the entire string as one sexp
  (read (concat "\"" str "\"")))

(defun aig--hash-comp (hash1 hash2 key &optional comp-fn)
  "Compares the key values of hash1 and hash2 using comp-fn, defaulting to equal if comp-fn was no supplied."
  (funcall (or comp-fn equal) (gethash key hash1) (gethash key hash2)))

(defun aig--make-directory-maybe (dirpath &optional parents)
  "Prompts the user yes or no whether to create the directory at dirpath.
If dirpath already exists, this function does nothing. The variable `parents'
is passed directly to the parents variable in make-directory.

Returns the created directory's path or nil if the user decline the creation of dirpath."
  ;;Use file exists to check if the directory exists, if not create it
  ;; if the directory exists, but is not writable, the make-directory will error
  (if (file-exists-p dirpath)
      dirpath
    (if (yes-or-no-p (format "\"%s\" does not exists, create it?" dirpath))
        (progn
          (make-directory dirpath parents) ;;make the directory
          dirpath)
      nil))) ;;the user decline the directory's creation

;;
;; Utility Functions
;;

;;
;; Macros
;;
(defmacro aig--extract (type ls)
  "From a list of packaged templates, returns a list of only `type' of each packaged template."
  `(mapcar #',(intern (concat "aig-template-" (symbol-name type))) ,ls))
;;
;; Macros
;;

;;
;; Loading Files and Parsing
;;

;;Parses the header of a template file and saves the parsed values in the hash-table
;; Handles errors as needed
(defun aig--parse-template-file-header (contents hash-table)
"Parses the header of a template file given the file's contents and saves the 
parsed values in the given hash-table. It signals an error if the header of the file is poorly formatted."
  (if (null contents) ;;if contents is null, that means there was no #-- end identifier
      (error "Missing end identifier to template file header")
    (let ((first (car contents))
          (rest (cdr contents)))
      ;;Error if the regex does not match anything, that means the header was poorly formed
      (cond 
          ((string-match 
            ;;matches ^#<whitespace>*<non-whitespace>*<whitespace>+\[<anything>*\]<whitespace>$
            "^#\\s-*\\(\\S-*\\)\\s-+\\[\\(.*\\)\\]\\s-*$" first) ;;key value pair matching
           ;;Add entry to the hash-table and recurse
           (progn
             ;;The first match is the key and the second match is the value
             (let ((key (match-string 1 first))
                   (val (match-string 2 first)))
               
               ;;If the first match is an empty string, ie: no non-whitespace characters
               ;; were found, then that line is interpreted as a comment line and is ignored
               (if (zerop (length key))
                   nil ;;do nothing
                 ;;Be sure to unescape the string so that chars like \n actually become newlines
                 ;; this makes it as though whatever is between the [] is interpreted as a normal string
                 ;; although being read character by character from the file input
                 (puthash key (aig--unescape-string val) hash-table))

               (aig--parse-template-file-header rest hash-table))))
          ((string-match "^#\\s-*--\\s-*$" first) ;;end identifier matching
           (cons hash-table rest)) ;;Return the hash table and the rest of the file's contents when done
          (t ;;Else, there must be an error
           (error "Invalid syntax on line: \"%s\"" first))))))

(defun aig--parse-template (contents)
  "Given a template's contents, this function parses it into a hashtable and returns it."
  ;;Each hash should guarantee to have certain fields in the rest of the program,
  ;; so do the error checking here to see if everything is alright
  (let* ((hash-table (make-hash-table :test 'aig--string=))
         (pair (aig--parse-template-file-header contents hash-table))
         (hash-template (car pair))
         (rest-contents (cdr pair)))
    ;;concatenate rest-contents with newlines after each
    ;; statement so that comments don't comment more than wanted
    ;; and insert it into the hash-template under the key: exec
    ;; make sure to wrap the rest contents in a progn to be able to evaluate it as one big sexp
    (puthash "exec" (mapconcat 'identity (append '("(progn ") rest-contents '(")")) "\n") hash-template)
    
    ;;lexical scoped cl-labels for safe measures and recursive elements
    (cl-labels ((key-exists (key) 
                            "Tests if key exists in hash-template, if list, at least one of the elems has to exist."
                            (if (listp key) ;;Then at least one of the keys inside the list has to exists
                                (cl-some #'key-exists key)
                              (gethash key hash-template nil)))
                (format-error (err-key)
                              "Makes the error message containing the missing key(s) more user friendly."
                              (if (listp err-key)
                                  ;;format it so that each element is quoted and has or's interspersed
                                  (cl-reduce #'(lambda (acc x)
                                                 (concat "\"" acc "\" or \"" x "\""))
                                             err-key)
                                ;;just quote the err text
                                (concat "\"" err-key "\""))))
      ;;We only care about the side-effect if it errors, so that is why we use mapc
      (mapc #'(lambda (key-name)
                "Errors when the key key-name does not exist in the hash-table."
                (if (key-exists key-name)
                    nil ;;no effect
                  ;;error if it is not found
                  (error "Parsed template does not include key %s" (format-error key-name))))
            '("name" ("hook" "hook-func") "delim" "expr" "exec")))
    ;;If mapc did not encounter an error, hash-template is valid and can be returned
    hash-template))

(defun aig--load-template-from-contents (mode contents &optional path)
  "Loads a template given its raw unprocessed contents into a given mode.
Also, it packages other pieces of data like its file path with the template."
  ;;Get what is in mode, defaulting to '() if there is nothing
  ;; and append the parsed data to that list
  ;;Then, place it back into the hash table
  ;;Also, contents has to be split around a newline for aig--parse-template
  (let* ((mode-contents (gethash mode aig--hash-templates-by-mode '()))
         (split-contents (split-string contents "\n"))
         (parsed-template (aig--parse-template split-contents))
         (packaged-template (make-aig-template :hash parsed-template :path path :loaded-mode mode)))
    ;;Add to the mode by removing any templates with the same name and replacing them with the new one
    ;; the list mode-contents is a list of packaged templates, only the hashes need to be compared in the lambda
    (puthash mode (cons packaged-template
                        (cl-remove parsed-template mode-contents
                                   :test (lambda (e1 e2) ;;e1 is a template hash while e2 is a packaged template
                                           "Tests if the name of elem matches that of the parsed template's name."
                                           (aig--hash-comp e1 (aig-template-hash e2) "name" #'string=))))
             aig--hash-templates-by-mode)))

(defun aig--get-mode-dirs (root-dir)
  "Gets all of the directories in root-dir that end in -mode."
  ;;root-contents is a list of files and directories ending in -mode with its full path
  ;; they are then later filtered to only directories by the attributes
  (let ((root-contents (directory-files-and-attributes root-dir t "-mode$" t)))
    ;;Filter so that only the directories are left from root-contents
    (cl-reduce #'(lambda (acc x) 
                   (if (cadr x)
                       (cons (car x) acc)
                     acc))
               root-contents
               :initial-value '())))

(defun aig--get-template-files (mode-dir)
  "Given a directory mode-dir, returns all of the files in it.
It is assumed every file in mode-dir is a template file."
  (let ((templates (directory-files-and-attributes mode-dir t nil t)))
    ;;Filter so that only the files are left from templates
    (cl-reduce #'(lambda (acc x) 
                   (if (not (cadr x))
                       (cons (car x) acc)
                     acc))
               templates
               :initial-value '())))

(defun aig--load-templates-from-root-dir (root-dir)
  "Loads all of the template files located in the sub-mode dirs of root-dir and returns nothing.
root-dir is expected to be an existing directory of all of the sub-mode dirs."

  ;;mode-dirs is a list of the full paths of directories containing templates
  (let ((mode-dirs (aig--get-mode-dirs root-dir)))
    (mapc #'(lambda (mode-dir) 
              "mode-dir is the full directory path of each sub-directory in 
root-dir that will contain template files"
              ;;template-files is a list of all of the templates with a full path found in mode-dir
              ;;mode is the mode each template file is to be activated in which is
              ;; the directory name of mode-dir without the full path
              ;; it is extracted by spliting the mode-dir about /, taking the last one via (car (last ...))
              (let ((template-files (aig--get-template-files mode-dir))
                    (mode (car (last (split-string mode-dir "/")))))
                (mapc #'(lambda (template-file-path)
                          ;;Loads the template at template-file-path into the mode and supplies its path
                          ;; into aig--load-template-from-contents
                          (aig--load-template-from-contents mode (aig--read-file template-file-path) 
                                                            template-file-path))
                      template-files)))
          mode-dirs)))

(defun aig--load-templates-from-dirs* (dirs-ls)
  "Loads all of the templates found in a list of directories. This function also handles errors."
  (if (null dirs-ls)
      nil ;;base case satisfied, return
    (let ((first (car dirs-ls))
          (rest (cdr dirs-ls)))
      (condition-case error-info
          (aig--load-templates-from-root-dir first) ;;load each individual directory
        (file-error (aig-error (format "%s" error-info))))
      (aig--load-templates-from-dirs* rest)))) ;;continue the recursion

(defun aig--load-templates-from-dirs (dirs-ls)
  "Clears the hash templates and loads all the templates found in a list of directories."
  ;;Clear the hash table every time the load function is called
  (clrhash aig--hash-templates-by-mode)
  (aig--load-templates-from-dirs* dirs-ls))

;;
;; Loading Files and Parsing
;;

;;
;; Catching and Processing Hooks
;;

(defun aig--get-templates-for-major-mode ()
  "Returns the list of template hash tables corresponding to the respective
value in major-mode. Returns '() if there are no templates."
  (gethash (symbol-name major-mode) aig--hash-templates-by-mode '()))

(defun aig--get-search-region (start-delim)
  "Gets the region of the buffer that is between the regex start-delim and point.
If start-delim is not satisfied, the beginning of the buffer is used."
  ;;make it a special let because point moves around when calling re-search-backward
  (let* ((end-point (point))
         (start-point (if (re-search-backward (concat "\\(" start-delim "\\)") nil t) ;;non-nil means matched
                          (match-end 1) ;;The end of the match
                        ;;No match means that everything before the point was exhausted, 
                        ;; so use the beginning of the buffer
                        1)))
    ;;move point to its original position
    (goto-char end-point)

    ;;Return the search area
    (buffer-substring start-point end-point)))

(defun aig-update-context ()
  "Updates the context list, hooked on post-command-hook."
  
  ;;clear the current context by setting aig--list-context-hash-templates to '()
  (setq aig--list-context-hash-templates '())

  ;;templates-ls is the list of templates for the current major-mode, '() if there are none
  (let ((templates-ls (aig--extract hash (aig--get-templates-for-major-mode))))
    (mapc #'(lambda (hash-template)
             (let ((search-area (aig--get-search-region (gethash "delim" hash-template))))
               ;;Destructive cons the hash-template if expr matches the context search-area
               ;; else do nothing
               (if (string-match (gethash "expr" hash-template) search-area)
                   (push hash-template aig--list-context-hash-templates)
                 nil)))
          templates-ls)))

(defun aig--hooks-match (hash)
  "Returns t if the hook is satisfied with the string value of (this-command-keys) or if
hook-func is satisfied with the string value of this-command. If both hook and hook-func are available,
then both must be satisfied by their respective string values."
  (let ((hook (gethash "hook" hash nil))
        (hook-func (gethash "hook-func" hash nil)))
    (and 
     (or (not hook) ;;compare the this-command-keys string value to hook if hook is non-nil
         (string= (format "%s" (this-command-keys)) hook))
     (or (not hook-func) ;;compare the this-command string value to hook-func if hook-func is non-nil
         (string= (format "%s" this-command) hook-func)))))

(defun aig-scan-context (context-hashes)
  "Scans the context to see if an hooks are satisfied, hooked on post-command-hook."
  (if (null context-hashes)
      '() ;;base case, return '() if context-hashes list is empty
    (let ((first (car context-hashes))
          (rest (cdr context-hashes)))

      ;;check if the hooks match
      (if (aig--hooks-match first)
          (cons first (aig-scan-context rest)) ;;cons the matched context template and continue the recursion
          (aig-scan-context rest))))) ;;continue the recursion 

(defun aig--eval-hash-template (hash-template)
  "Evaluates the exec entry in a given hashtable. This function catches and displays any
error that may occur during the execution."
  (condition-case error-info
      (let ((exec-str (gethash "exec" hash-template)))
        (eval (read exec-str)))
    (error (aig-error (format "In evaluating lisp expr in \"%s\": %s" (gethash "name" hash-template) error-info)))))

(defun aig--post-command-hook-handle ()
  "The handle hooked to the post-command-hook. It first tries to satisfy any hooks
of the scanned-context that had a matching expr before the input of the current character.
It then updates the contexts within the buffer."
  (let* ((scanned-contexts (aig-scan-context aig--list-context-hash-templates))
         (contexts-len (length scanned-contexts)))
    ;;handles if there was a match
    (cond
     ((zerop contexts-len) nil) ;;no matches
     ((= 1 contexts-len) (aig--eval-hash-template (car scanned-contexts))) ;;single match, execute it
     (t  ;;more than one match, prompt for which one to use
      (let* ((choices (mapcar (lambda (elem) (gethash "name" elem)) scanned-contexts))
             (selected-context (aig-prompt "(AIG) Multiple matches found: " choices t))) ;;also, require a match
        ;;if the selected-context is nil, then most likely C-g was hit so ignore it
        ;; else, eval the context found at the same index selected-context is found in choices
        (if (not selected-context)
            nil
          (aig--eval-hash-template
           (nth (cl-position selected-context choices :test #'string=) scanned-contexts)))))))

  ;;update the context after the scanning of satisfied hooks is complete
  (aig-update-context))

;;
;; Catching and Processing Hooks
;;

;;
;; Error Messaging
;;
(defun aig-error (err-msg)
  "It displays an error message with a custom prefix."
  (message (concat "AIG Error: " err-msg)))
;;
;; Error Messaging
;;

;;
;; Prompting Menu
;;
(defun aig-ido-prompt (prompt choices req-match)
  "Safely tries to run ido as a prompting method.
Returns nil if it was not able or the selected match."
  ;;make sure ido is available
  (if (and (fboundp #'ido-completing-read)
           (or (>= emacs-major-version 24)
               ido-mode))
      (ido-completing-read prompt choices nil req-match) ;;prompt if possible
    nil)) ;;return nil signifying completing-prompt was not able to prompt

(defun aig-completing-prompt (prompt choices req-match)
  "Safely tries to run completing-read as a prompting method.
Returns nil if it was not able or the selected match."
  ;;make sure completing-read is available
  (if (fboundp #'completing-read)
      (completing-read prompt choices nil req-match) ;;prompt if possible
    nil)) ;;return nil signifying completing-read was not able to prompt

(defun aig-first-choice-no-prompt (prompt choices req-match)
  "Returns the first option of the list of choices.
Returns nil if an empty list of choices was supplied or the selected match."
  ;;simply return the first choice without prompting
  ;; automatically returning nil if there are no choices
  (car-safe choices))

(defun aig--prompt* (prompt choices req-match)
  "Given a prompt and a list of string choices, returns the selected choice
 or nil if the prompting functions in the list aig-prompt-functions all failed.

If req-match is non-nil, then the prompting methods should not allow the user to 
input a choice that is not in choices."
  ;;The result is the first non-nil return of the functions found
  ;; in the list aig-prompt-functions, returns nil if all of the
  ;; prompting methods failed or C-g was hit
  (with-local-quit
    (cl-some (lambda (fun)
               (funcall fun prompt choices req-match))
             aig-prompt-functions)))

(defun aig-prompt (prompt choices &optional req-match)
  "Prompts the user with prompt and a list of strings of possible choices. It tries the 
prompting methods in the list `aig-prompt-functions' until one of the successfully returns.

If req-match is non-nil, then the prompting methods should not allow the user to 
input a choice that is not in choices."
  ;;Save the hook-state so that it can be set to it after disabling
  (let ((hook-state (aig-get-post-command-hook-state)))
    ;;Disable the post-command-hook temporarily because the promoting function interfere with it
    (aig-disable-post-command-hook)

    (let ((result (aig--prompt* prompt choices req-match)))
      (aig-set-post-command-hook-state hook-state) ;;set the state hook at the end
      result))) ;;Return the result
;;
;; Prompting Menu
;;

;;
;; User Interactive Functions
;;
(defun aig--init-template-buffer (mode buffer-template)
  "Sets up the current buffer to be a template buffer with the suggested mode as `mode'."
  ;;set up the buffer local variables
  (setq-local aig--guessed-modes (list (symbol-name mode)))
  (setq-local aig--current-buffer-template-buffer t)
  (setq-local aig--current-buffer-template buffer-template)
  
  ;;enable aig--template-mode in the new template buffer for key-bindings and other functionalities
  (aig--template-mode 1))

(defun aig-new-template ()
  "Creates a new template buffer."
  (interactive)

  (let ((mode major-mode))
    ;;Generate a new template buffer and switch to it
    (switch-to-buffer-other-window (generate-new-buffer aig--new-template-buffer-name))

    ;;Set up this current buffer as a template buffer with mode as the suggested mode for loading
    ;; since this is a new template, no buffer-template can be specified
    (aig--init-template-buffer mode nil)

    ;;Insert the starter template into the new template buffer
    (insert aig--new-template-buffer-template)))

(defun aig-load-template-buffer (&optional mode)
  "Loads the current template buffer into a mode.
If mode is not provided, this function prompts the user."
  (interactive)

  ;;Basic sanity check, if the current buffer is a template buffer, then load it
  (if aig--current-buffer-template-buffer
      (let ((selected-mode (or mode (aig-prompt "Select mode to load into: " aig--guessed-modes))))
        ;;If `selected-mode' is nil, that means most likely C-g was used to exit, so exit doing nothing
        (if selected-mode
            ;;load the visible buffer into selected-mode, since this is only loading
            ;; no path can be supplied to aig--load-template-from-contents as a file path does not exist
            (aig--load-template-from-contents selected-mode (buffer-substring-no-properties
                                                             (point-min) (point-max)) nil)
          nil))
    nil))

(defun aig-load-templates ()
  "Loads all of the templates found in the sub-mode directories located in
each directory within the list `aig-template-dirs'."
  (interactive)
  (aig--load-templates-from-dirs aig-template-dirs))

(defun aig-load-directory ()
  "Loads a directory into the `aig--hash-templates-by-mode' by interactively promoting for a directory."
  (interactive)
  (let ((dir (read-directory-name "Select a directory to load: " nil nil t)))
    ;;Use aig--load-templates-from-dirs* as it handles errors internally
    (aig--load-templates-from-dirs* (list dir))))

(defun aig--switch-to-template-buffer (template)
  "Switches to the template buffer of `template' if it is already opened, if not, it opens
the file of `template' and initializes the buffer as a template buffer."
  (let* ((filepath (aig-template-path template))
         (opened-buffer (get-file-buffer filepath)))
    ;;If the filepath is opened in a buffer, just switch to it, else
    ;; read and load the path as a new template buffer
    (if opened-buffer
        (switch-to-buffer-other-window opened-buffer)
      (let ((mode major-mode))
        ;;Open the file in the other window
        (find-file-other-window filepath)
        
        ;TODO: Check if when coping, the hash table does not point to the same object or if it
        ;; also gets copied

        ;;Set up this current buffer as a template buffer with mode as the suggested mode for loading
        ;; copy the template and its contents because it is not desirable to have the
        ;; buffer template variable point directly to a loaded template
        (aig--init-template-buffer mode (copy-aig-template template))))))

(defun aig-visit-template-file ()
  "Opens a template file loaded in the current major mode for editing."
  (interactive)

  ;;loaded-templates are the packaged templates of the current major-mode in a list
  ;; template-hashes is just a list of the hash component of each of the loaded templates
  ;; template-names is a list of the names of each of the template hashes
  ;;Note: all the templates share indexes, so the first element of loaded-template has a hash of
  ;; the first of template-hashes which has a \"name\" field of the first of template-names
  (let* ((loaded-templates (aig--get-templates-for-major-mode))
         (template-hashes (aig--extract hash loaded-templates))
         (template-names (mapcar #'(lambda (x) 
                                     (gethash "name" x)) 
                                 template-hashes))
         ;;Prompt giving the names, requiring a match from the template-names choices
         (selected-template-name (aig-prompt "Choose a template to edit: " template-names t)))
      ;;If selected-template-name is nil, that means most likely C-g was activated, so do nothing
      (if selected-template-name
          ;;At the same index the selected-template-name is in template-names, the selected packaged template is
          ;; in the loaded-template
          (let ((selected-template (nth (cl-position selected-template-name template-names :test #'string=)
                                         loaded-templates))) 
            ;;aig--switch-to-template-buffer will then handle the file
            ;; opening and initializing the buffer as a template buffer
            (aig--switch-to-template-buffer selected-template))
        nil)))

(defun aig-load-save-close-template-buffer ()
  "This function loads the current buffer as a template buffer, saves it, prompting for a path
and creating mode directories as needed if the current template buffer does not have a path
attributed. After the loading processes passed, it then closes the buffer."
  (interactive)

    ;;Basic sanity check, if the current buffer is a template buffer
  (when aig--current-buffer-template-buffer
      ;;If the current template buffer already has a filepath and mode, just load the template, save, and close
      ;; usually aig--current-buffer-template is this full if it is a visited template
      (if (and aig--current-buffer-template 
               (aig-template-path aig--current-buffer-template)
               (aig-template-loaded-mode aig--current-buffer-template))
          (progn
            ;;load
            (aig-load-template-buffer (aig-template-loaded-mode aig--current-buffer-template))
            ;;save
            (set-visited-file-name (aig-template-path aig--current-buffer-template))
            (save-buffer 0) ;;save without backups
            ;;close
            (kill-buffer))
        (progn ;;prompt the user for where to save, load it to that location, save, and close
          ;;lexical scope flet for safety
          (cl-flet 
              ((get-save-dir (mode)
                             "Returns the path to the directory to save the template buffer or nil to exit."
                             (let ((selected-root (aig-prompt "Select a root directory to save into: " 
                                                              aig-template-dirs)))
                               ;;most likely C-g was hit to exit or the dirpath creation failed
                               ;; to cause this to exit
                               (when selected-root
                                 ;;make the selected-root + mode path if needed, prompting the user
                                 ;; before its creation, make any parent dirs as needed
                                 (aig--make-directory-maybe (concat (file-name-as-directory selected-root) mode) 
                                                            t)))))
            
            ;;Sequentially prompt the user for information, if one of the function fails or the user
            ;; hits C-g to exit, its value will be set to nil and all following variables will 
            ;; be automatically set to nil also
            (let* ((selected-mode (aig-prompt "Select mode to load into: " aig--guessed-modes))
                   (dirpath (when selected-mode (get-save-dir selected-mode)))
                   (save-as (when dirpath (aig-prompt "Save template as: " '()))))
              ;;if save as is defined, that means all of the other variables defined before it
              ;; must also be defined
              (when save-as
                ;;load
                (aig-load-template-buffer selected-mode)
                ;;save
                (set-visited-file-name (concat (file-name-as-directory dirpath) save-as))
                (save-buffer 0) ;;save without backups
                ;;close
                (kill-buffer))))))))

(defun aig-about ()
  "Returns the about information for Autoinsertion Guru."
  (interactive)
  (message (format "%s %s -- %s" aig--about-name aig--version (apply #'concat aig--developers))))

;;
;; User Interactive Functions
;;

;;
;; Hooks
;;
(defun aig-enable-post-command-hook ()
  "Enables the aig--post-command-hook-handle on post-command-hook buffer locally."
  (add-hook 'post-command-hook #'aig--post-command-hook-handle nil t))

(defun aig-disable-post-command-hook ()
  "Disables the aig--post-command-hook-handle on post-command-hook buffer locally."
  (remove-hook 'post-command-hook #'aig--post-command-hook-handle t))

(defun aig-get-post-command-hook-state ()
  "Returns non-nil if aig is hooked to post-command-hook and nil if it is not."
  (cl-position 'aig--post-command-hook-handle post-command-hook))

(defun aig-set-post-command-hook-state (state)
  "If state is non-nil, aig is hooked to the post-command-hook, else it is un-hooked."
  (if state
      (aig-enable-post-command-hook)
    (aig-disable-post-command-hook)))
;;
;; Hooks
;;

;;
;; Temporary calls
;;

(aig-enable-post-command-hook)
(aig-disable-post-command-hook)

(setq aig-template-dirs
      (cons "/home/damian/bin/ELisp_files/autoinsertion_guru/sample-modes"
            (remove "/home/damian/bin/ELisp_files/autoinsertion_guru/sample-modes"
                    aig-template-dirs)))
(aig--load-templates-from-dirs* '("/home/damian/bin/ELisp_files/autoinsertion_guru/sample-modes"))

;;
;; Temporary calls
;;

;;Provide that this extension was loaded
(provide 'autoinsertion-guru)
