
(require 'cl-lib)

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
  "List of guessed modes supporting `aig-load-template-buffer', set as a buffer local variable.")

(defvar aig--current-buffer-template-buffer nil
  "Boolean if the current buffer is a template buffer, set as a buffer local variable.")

(defconst aig--new-template-buffer-name "*new aig template*" 
  "The name of the buffer that is created when making a new template.")

(defconst aig--new-template-buffer-template
  "# name [template name]
# [this is a sample comment]
# hook [hook character]
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

(defun aig--index-member (obj ls)
  "Returns the index were obj was found in ls or nil if it was not found using member internally."
  (let ((found (member obj ls)))
    (if (not found)
        nil
      (- (length ls) (length found))))) ;;A small hack to get the index where obj was found

(defun aig--cons-if (elem ls predicate)
  "Cons elem to ls if predicate returns a nil values when mapped over all the elements in ls.
Returns the newly cons'd list, else just the list."
  (if (cl-some predicate ls)
      ls ;;predicate returned a non-nil for at least one element in ls, return ls only
    (cons elem ls))) ;;predicate failed on all elements, so cons

;;
;; Utility Functions
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
  (let ((hash-table (make-hash-table :test 'aig--string=)))
    ;;Each hash should guarantee to have the following fields: name, hook, delim, expr, exec
    ;; in the rest of the program, so do the error checking here
    (let* ((pair (aig--parse-template-file-header contents hash-table))
           (hash-template (car pair))
           (rest-contents (cdr pair)))
      ;;concatenate rest-contents with newlines after each
      ;; statement so that comments don't comment more than wanted
      ;; and insert it into the hash-template under the key: exec
      ;; make sure to wrap the rest contents in a progn to be able to evaluate it as one big sexp
      (puthash "exec" (mapconcat 'identity (append '("(progn ") rest-contents '(")")) "\n") hash-template)

      ;;We only care about the side-effect if it errors, so that is why we use mapc
      (mapc #'(lambda (key-name) 
                (if (gethash key-name hash-template nil) ;;returns nil if the key-name does not exist
                    '()
                  (error "Parsed template does not include key \"%s\"" key-name))) ;;error if it is not found
            '("name" "hook" "delim" "expr" "exec"))
      ;;If mapc did not encounter an error, hash-template is valid and can be returned
      hash-template)))

(defun aig--load-template-from-contents (mode contents)
  "Loads a template given its raw unprocessed contents into a given mode."
  ;;Get what is in mode, defaulting to '() if there is nothing
  ;; and append the parsed data to that list if the predicate fails
  ;; on all the elements already in the mode's contents.
  ;; Then, place it back into the hash table
  ;; Also, contents has to be split around a newline for aig--parse-template
  (let* ((mode-contents (gethash mode aig--hash-templates-by-mode '()))
         (split-contents (split-string contents "\n"))
         (parsed-template (aig--parse-template split-contents))
         (parsed-template-name (gethash "name" parsed-template)))
    (puthash mode (aig--cons-if parsed-template mode-contents 
                                (lambda (elem) 
                                  "Tests if the name of elem matches that of the parsed template's name."
                                  (string= (gethash "name" elem) parsed-template-name)))
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
                          (aig--load-template-from-contents mode (aig--read-file template-file-path)))
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
  (let ((templates-ls (aig--get-templates-for-major-mode)))
    (mapc #'(lambda (hash-template)
             (let ((search-area (aig--get-search-region (gethash "delim" hash-template))))
               ;;Destructive cons the hash-template if expr matches the context search-area
               ;; else do nothing
               (if (string-match (gethash "expr" hash-template) search-area)
                   (push hash-template aig--list-context-hash-templates)
                 nil)))
          templates-ls)))

(defun aig--hook-matches-last-input-event (hook)
  "Returns t if the last-input-event matches the hook, else it returns nil."
  (cond
   ((symbolp last-input-event) ;;keyboard keys like arrows, return, etc.
    (string= (symbol-name last-input-event) hook))
   ;;if the last-input-event is a number, that means a char was input, so
   ;; in order to match, hook must be a string of length 1
   ((and (numberp last-input-event) (= 1 (length hook)))
    (= last-input-event (string-to-char hook)))
   (t nil))) ;;no match found

(defun aig-scan-context (context-hashes)
  "Scans the context to see if an hooks are satisfied, hooked on post-command-hook."
  (if (null context-hashes)
      '() ;;base case, return '() if context-hashes list is empty
    (let ((first (car context-hashes))
          (rest (cdr context-hashes)))

      ;;check if the hook matched
      (if (aig--hook-matches-last-input-event (gethash "hook" first))
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
             (selected-context (aig-prompt "(AIG) Multiple matches found: " choices)))
        ;;if the selected-context is nil, then most likely C-g was hit so ignore it
        ;; else, eval the context found at the same index selected-context is found in choices
        (if (not selected-context)
            nil
          (aig--eval-hash-template (nth (aig--index-member selected-context choices) scanned-contexts)))))))
  
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
(defun aig-ido-prompt (prompt choices)
  "Safely tries to run ido as a prompting method.
Returns nil if it was not able or the selected match."
  ;;make sure ido is available
  (if (and (fboundp #'ido-completing-read)
           (or (>= emacs-major-version 24)
               ido-mode))
      (ido-completing-read prompt choices) ;;prompt if possible
    nil)) ;;return nil signifying completing-prompt was not able to prompt

(defun aig-completing-prompt (prompt choices)
  "Safely tries to run completing-read as a prompting method.
Returns nil if it was not able or the selected match."
  ;;make sure completing-read is available
  (if (fboundp #'completing-read)
      (completing-read prompt choices) ;;prompt if possible
    nil)) ;;return nil signifying completing-read was not able to prompt

(defun aig-first-choice-no-prompt (prompt choices)
  "Returns the first option of the list of choices.
Returns nil if an empty list of choices was supplied or the selected match."
  ;;simply return the first choice without prompting
  ;; automatically returning nil if there are no choices
  (car-safe choices))

(defun aig--prompt* (prompt choices)
  "Given a prompt and a list of string choices, returns the selected choice
 or nil if the prompting functions in the list aig-prompt-functions all failed."
  ;;The result is the first non-nil return of the functions found
  ;; in the list aig-prompt-functions, returns nil if all of the
  ;; prompting methods failed or C-g was hit
  (with-local-quit
    (cl-some (lambda (fun)
               (funcall fun prompt choices))
             aig-prompt-functions)))

(defun aig-prompt (prompt choices)
  "Prompts the user with prompt and a list of strings of possible choices. It tries the 
prompting methods in the list `aig-prompt-functions' until one of the successfully returns."
  ;;Save the hook-state so that it can be set to it after disabling
  (let ((hook-state (aig-get-post-command-hook-state)))
    ;;Disable the post-command-hook temporarily because the promoting function interfere with it
    (aig-disable-post-command-hook)

    (let ((result (aig--prompt* prompt choices)))
      (aig-set-post-command-hook-state hook-state) ;;set the state hook at the end
      result))) ;;Return the result
;;
;; Prompting Menu
;;

;;
;; User functions
;;
(defun aig-new-template ()
  "Creates a new template buffer."
  (interactive)

  ;;Generate a new template buffer and switch to it
  (switch-to-buffer-other-window (generate-new-buffer aig--new-template-buffer-name))

  ;;set up the buffer local variables
  (setq-local aig--guessed-modes (list (symbol-name major-mode)))
  (setq-local aig--current-buffer-template-buffer t)
  
  ;;enable aig-minor-mode in the new template buffer
  (aig-minor-mode 1)

  ;;insert the starter template into the new template buffer
  (insert aig--new-template-buffer-template)
  )

(defun aig-load-template-buffer ()
  "Loads the current template buffer into a selected mode."
  (interactive)

  ;;If the current buffer is a template buffer, then load it
  (if aig--current-buffer-template-buffer
      (let ((selected-mode (aig-prompt "Select mode to load into: " aig--guessed-modes)))
        ;;If mode is nil, that means most likely C-g was used to exit, so exit doing nothing
        (if selected-mode
            ;;load the visible buffer into selected-mode
            (aig--load-template-from-contents selected-mode (buffer-substring-no-properties
                                                             (point-min) (point-max)))
          nil))
    nil))

(defun aig-load-templates ()
  "Loads all of the templates found in the sub-mode directories located in
each directory within the list `aig-template-dirs'."
  (interactive)
  ;TODO: somehow handle missing directories
  (aig--load-templates-from-dirs aig-template-dirs))

(defun aig-load-directory ()
  "Loads a directory into the `aig--hash-templates-by-mode' by interactively promoting for a directory."
  (interactive)
  (let ((dir (read-directory-name "Select a directory to load: " nil nil t)))
    ;;Use aig--load-templates-from-dirs* as it handles errors internally
    (aig--load-templates-from-dirs* (list dir))))

;;
;; User functions
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
  (aig--index-member #'aig--post-command-hook-handle post-command-hook))

(defun aig-set-post-command-hook-state (state)
  "If state is non-nil, aig is hooked to the post-command-hook, else it is un-hooked."
  (if state
      (aig-enable-post-command-hook)
    (aig-disable-post-command-hook)))
;;
;; Hooks
;;

;(char-to-string 43)
;(read-event)

(aig-enable-post-command-hook)
(aig-disable-post-command-hook)

(aig--load-templates-from-dirs '("."))

;;Provide that this extension was loaded
(provide 'autoinsertion-guru)
