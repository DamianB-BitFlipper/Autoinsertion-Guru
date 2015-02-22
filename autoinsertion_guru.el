
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
templates for each respective mode identified by the key

Each hash table element in each of the lists contained in this hash table is
guaranteed to have the following fields: name, hook, delim, expr
If on loading, those fields are missing, an error will be signaled")

(defvar aig--list-context-hash-templates '() 
  "A list of hashes of templates that are enabled in the current context
This variable is updated every time the context is updated")

;;
;; Global variables
;;

;;
;; Modes
;;

;;;###autoload
(define-minor-mode aig-minor-mode
  "Toggle Auto-insertion Guru mode

When Auto-insertion Guru is enabled, it listens and tries to evaluate
hooks defined in the templates of the currently loaded major-mode"
  nil
  ;;The indicator for the mode line
  " aig"
  :group 'autoinsertion-guru
  (cond 
   (aig-minor-mode (progn
                     ;;Enable the post-command-hook
                     (aig-enable-post-command-hook)

                     ;;If the aig--hash-templates-by-mode is empty, then load the
                     ;; hash templates
                     (if (zerop (hash-table-count aig--hash-templates-by-mode))
                         ;;TODO: Make this load from the configurable list of directories
                         (aig-load-templates-from-dirs '("/home/damian/bin/ELisp_files/autoinsertion_guru/"))
                       nil)))
   (t (progn
        ;;Disable the post-command-hook
        (aig-disable-post-command-hook)))))

;;By default, do not activate aig in the minibuffer
(defvar aig-dont-activate '(minibufferp)
  "List of functions which if evaluated returns t, suppresses the activation of 
Autoinsertion Guru in that buffer. Functions should take 0 arguments.")

(defun aig-minor-mode-on ()
  "Turns on the Autoinsertion Guru minor mode, respecting `aig-dont-activate'."
  (if (cl-some #'funcall aig-dont-activate)
      nil ;;do not activate if one of the functions in aig-dont-activate returned t
      (aig-minor-mode 1))) ;;activate if all of the functions in aig-dont-activate returned nil

;;;###autoload
(define-globalized-minor-mode aig-global-mode aig-minor-mode aig-minor-mode-on
  :group 'autoinsertion-guru
  :require 'autoinsertion-guru)

;;
;; Modes
;;

;;
;; Utility Functions
;;

;;Read the contents of a file as a list of string where each string is a line of the file
(defun aig--read-lines (file-path)
  "Return a list of lines of a file at file-path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun aig--unescape-string (str)
  "Takes a string with escape chars and makes them into their equivalents processed
ie: \n (2 chars) becomes newline (1 char)"
  ;;add quotes around the string so that read interprets the entire string as one sexp
  (read (concat "\"" str "\"")))

(defun aig--index-member (str ls)
  "Returns the index were str was found in ls or nil if it was not found using member internally."
  (let ((found (member str ls)))
    (if (not found)
        nil
      (- (length ls) (length found))))) ;;A small hack to get the index where str was found

;;
;; Utility Functions
;;

;;
;; Loading Files and Parsing
;;

;;Parses the header of a template file and saves the parsed values in the hash-table
;; Handles errors as needed
(defun aig--parse-template-file-header (contents hash-table)
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

(defun aig--parse-template-file (template-file-path)
  "Given a template file path, this function parses it"
  (let ((contents (aig--read-lines template-file-path))
        (hash-table (make-hash-table :test 'aig--string=)))
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

(defun aig--load-template-file (mode template-file-path)
  "Loads a template file into a given mode"
  ;;Get what is in mode, defaulting to '() if there is nothing
  ;; and append the parsed data to that list and place it back into the hash table
  (let ((mode-contents (gethash mode aig--hash-templates-by-mode '())))
    (puthash mode (cons (aig--parse-template-file template-file-path) mode-contents)
             aig--hash-templates-by-mode)))

(defun aig--get-mode-dirs (root-dir)
  "Gets all of the directories in root-dir that end in -mode"
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
  "Given a directory mode-dir, returns all of the files in it
It is assumed every file in mode-dir is a template file"
  (let ((templates (directory-files-and-attributes mode-dir t nil t)))
    ;;Filter so that only the files are left from templates
    (cl-reduce #'(lambda (acc x) 
                   (if (not (cadr x))
                       (cons (car x) acc)
                     acc))
               templates
               :initial-value '())))

(defun aig--load-templates-from-root-dir (root-dir)
  "Loads all of the template files located in the sub-mode dirs of root-dir and returns nothing
root-dir is expected to be an existing directory of all of the sub-mode dirs"

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
                          (aig--load-template-file mode template-file-path))
                      template-files)))
          mode-dirs)))

(defun aig--load-templates-from-dirs* (dirs-ls)
  "Loads all of the templates found in a list of directories"
  (if (null dirs-ls)
      nil ;;base case satisfied, return
    (let ((first (car dirs-ls))
          (rest (cdr dirs-ls)))
      (aig--load-templates-from-root-dir first) ;;load each individual directory
      (aig--load-templates-from-dirs* rest)))) ;;continue the recursion

(defun aig-load-templates-from-dirs (dirs-ls)
  "Clears the hash templates and loads all the templates found in a list of directories"
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
value in major-mode. Returns '() if there are no templates"
  (gethash (symbol-name major-mode) aig--hash-templates-by-mode '()))

(defun aig--get-search-region (start-delim)
  "Gets the region of the buffer that is between the regex start-delim and point.
If start-delim is not satisfied, the beginning of the buffer is used"
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
  "Updates the context list, hooked on post-command-hook"
  
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
  "Returns t if the last-input-event matches the hook, else it returns nil"
  (cond
   ((symbolp last-input-event) ;;keyboard keys like arrows, return, etc.
    (string= (symbol-name last-input-event) hook))
   ;;if the last-input-event is a number, that means a char was input, so
   ;; in order to match, hook must be a string of length 1
   ((and (numberp last-input-event) (= 1 (length hook)))
    (= last-input-event (string-to-char hook)))
   (t nil))) ;;no match found

(defun aig-scan-context (context-hashes)
  "Scans the context to see if an hooks are satisfied, hooked on post-command-hook"
  (if (null context-hashes)
      '() ;;base case, return '() if context-hashes list is empty
    (let ((first (car context-hashes))
          (rest (cdr context-hashes)))

      ;;check if the hook matched
      (if (aig--hook-matches-last-input-event (gethash "hook" first))
          (cons first (aig-scan-context rest)) ;;cons the matched context template and continue the recursion
          (aig-scan-context rest))))) ;;continue the recursion 

(defun aig--eval-hash-template (hash-template)
  (let ((exec-str (gethash "exec" hash-template)))
    (eval (read exec-str))))

(defun aig--post-command-hook-handle ()
  (let* ((scanned-contexts (aig-scan-context aig--list-context-hash-templates))
         (contexts-len (length scanned-contexts)))
    ;;handles if there was a match
    (cond
     ((zerop contexts-len) nil) ;;no matches
     ((= 1 contexts-len) (aig--eval-hash-template (car scanned-contexts))) ;;single match, execute it
     (t  ;;more than one match, prompt for which one to use
      (let* ((choices (mapcar (lambda (elem) (gethash "name" elem)) scanned-contexts))
             (contexts-index (aig-prompt "(AIG) Multiple matches found: " choices)))
        ;;if the contexts-index is nil, then most likely C-g was hit so ignore it
        ;; else, eval the context at that index
        (if (not contexts-index)
            nil
          (aig--eval-hash-template (nth contexts-index scanned-contexts)))))))

  ;;update the context after the scanning of satisfied hooks is complete
  (aig-update-context))

;;
;; Catching and Processing Hooks
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
  (if (null choices) 
      nil ;;no choices, so exit with nil

    ;;The result is the first non-nil return of the functions found
    ;; in the list aig-prompt-functions, returns nil if all of the
    ;; prompting methods failed or C-g was hit
    (let ((result (with-local-quit
                    (cl-some (lambda (fun)
                               (funcall fun prompt choices))
                             aig-prompt-functions))))
      (if (not result)
          nil ;;All of the prompting methods failed, so exit with nil
        (aig--index-member result choices))))) ;;return the index the result is found in choices

(defun aig-prompt (prompt choices)
  ;;Disable the post-command-hook temporarily because the promoting function interfere with it
  (aig-disable-post-command-hook)

  (let ((result (aig--prompt* prompt choices)))
    (aig-enable-post-command-hook) ;;Re-enable the post-command-hook after disabling it
    result)) ;;Return the result

;;
;; Prompting Menu
;;

;;
;; Hooks
;;
(defun aig-enable-post-command-hook ()
  "Enables the aig--post-command-hook-handle on post-command-hook"
  (add-hook 'post-command-hook #'aig--post-command-hook-handle))

(defun aig-disable-post-command-hook ()
  "Disables the aig--post-command-hook-handle on post-command-hook"
  (remove-hook 'post-command-hook #'aig--post-command-hook-handle))
;;
;; Hooks
;;

;(char-to-string 43)
;(read-event)

(aig-enable-post-command-hook)
(aig-disable-post-command-hook)

;;Provide that this extension was loaded
(provide 'autoinsertion-guru)
