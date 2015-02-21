
(require 'cl-lib)

;;
;; User customizable variables
;;
(defgroup autoinsertion-guru nil
  "Automatically insert mini-snippets based on the context of the cursor."
  :group 'editing)

(defcustom aig-template-dirs (list "~/.emacs.d/aig-templates")
  "The directories where aig templates are search for in."
  :type '(choice (string :tag "Single directory (string)")
                 (repeat :args (string) :tag "List of directories (strings)"))
  :group 'autoinsertion-guru
  :require 'autoinsertion-guru)

;TODO: make the -20 char look-back on scan hooks a custom variable

;;
;; User customizable variables
;;

;;
;; Initialization
;;
;;make a hash table test named string= using string=
(define-hash-table-test 'string= 'string= 'sxhash)
;;
;; Initialization
;;

;;
;; Global variables
;;
(defvar hash-templates-by-mode (make-hash-table :test 'string=)
  "A hash table that holds a list of hash tables of loaded 
templates for each respective mode identified by the key

Each hash table element in each of the lists contained in this hash table is
guaranteed to have the following fields: name, hook, delim, expr
If on loading, those fields are missing, an error will be signaled")

(defvar list-context-hash-templates '() 
  "A list of hashes of templates that are enabled in the current context
This variable is updated every time the context is updated")

;;
;; Global variables
;;

;;Read the contents of a file as a list of string where each string is a line of the file
(defun read-lines (file-path)
  "Return a list of lines of a file at file-path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun unescape-string (str)
  "Takes a string with escape chars and makes them into their equivalents processed
ie: \n (2 chars) becomes newline (1 char)"
  ;;add quotes around the string so that read interprets the entire string as one sexp
  (read (concat "\"" str "\"")))

;;Parses the header of a template file and saves the parsed values in the hash-table
;; Handles errors as needed
(defun parse-template-file-header (contents hash-table)
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
                 (puthash key (unescape-string val) hash-table))

               (parse-template-file-header rest hash-table))))
          ((string-match "^#\\s-*--\\s-*$" first) ;;end identifier matching
           (cons hash-table rest)) ;;Return the hash table and the rest of the file's contents when done
          (t ;;Else, there must be an error
           (error "Invalid syntax on line: \"%s\"" first))))))

(defun parse-template-file (template-file-path)
  "Given a template file path, this function parses it"
  (let ((contents (read-lines template-file-path))
        (hash-table (make-hash-table :test 'string=)))
    ;;Each hash should guarantee to have the following fields: name, hook, delim, expr, exec
    ;; in the rest of the program, so do the error checking here
    (let* ((pair (parse-template-file-header contents hash-table))
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

(defun load-template-file (mode template-file-path)
  "Loads a template file into a given mode"
  ;;Get what is in mode, defaulting to '() if there is nothing
  ;; and append the parsed data to that list and place it back into the hash table
  (let ((mode-contents (gethash mode hash-templates-by-mode '())))
    (puthash mode (cons (parse-template-file template-file-path) mode-contents)
             hash-templates-by-mode)))

(defun get-mode-dirs (root-dir)
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

(defun get-template-files (mode-dir)
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

(defun load-templates-from-root-dir (root-dir)
  "Loads all of the template files located in the sub-mode dirs of root-dir
root-dir is expected to be an existing directory of all of the sub-mode dirs"
  ;;Clear the hash table every time the load function is called
  (clrhash hash-templates-by-mode)

  ;;mode-dirs is a list of the full paths of directories containing templates
  (let ((mode-dirs (get-mode-dirs root-dir)))
    (mapc #'(lambda (mode-dir) 
              "mode-dir is the full directory path of each sub-directory in 
root-dir that will contain template files"
              ;;template-files is a list of all of the templates with a full path found in mode-dir
              ;;mode is the mode each template file is to be activated in which is
              ;; the directory name of mode-dir without the full path
              ;; it is extracted by spliting the mode-dir about /, taking the last one via (car (last ...))
              (let ((template-files (get-template-files mode-dir))
                    (mode (car (last (split-string mode-dir "/")))))
                (mapc #'(lambda (template-file-path)
                          (load-template-file mode template-file-path))
                      template-files)))
          mode-dirs)))

(defun get-templates-for-major-mode ()
  "Returns the list of template hash tables corresponding to the respective
value in major-mode. Returns '() if there are no templates"
  (gethash (symbol-name major-mode) hash-templates-by-mode '()))

(defun aig-get-search-region (start-delim)
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
  
  ;;clear the current context by setting list-context-hash-templates to '()
  (setq list-context-hash-templates '())

  ;;templates-ls is the list of templates for the current major-mode, '() if there are none
  (let ((templates-ls (get-templates-for-major-mode)))
    (mapc #'(lambda (hash-template)
             (let ((search-area (aig-get-search-region (gethash "delim" hash-template))))
               ;;Destructive cons the hash-template if expr matches the context search-area
               ;; else do nothing
               (if (string-match (gethash "expr" hash-template) search-area)
                   (push hash-template list-context-hash-templates)
                 nil)))
          templates-ls)))

(defun hook-matches-last-input-event (hook)
  "Returns t if the last-input-event matches the hook, else it returns nil"
  (cond
   ((symbolp last-input-event)
    (string= (symbol-name last-input-event) hook))
   ((numberp last-input-event)
    (= last-input-event (string-to-char hook)))
   (t (error "Unknown input event %s" last-input-event))))

(defun aig-scan-context (context-hashes)
  "Scans the context to see if an hooks are satisfied, hooked on post-command-hook"
  (if (null context-hashes)
      '() ;;base case, return '() if context-hashes list is empty
    (let ((first (car context-hashes))
          (rest (cdr context-hashes)))

      ;;check if the hook matched
      (if (hook-matches-last-input-event (gethash "hook" first))
          (cons first (aig-scan-context rest)) ;;cons the matched context template and continue the recursion
          (aig-scan-context rest)) ;;continue the recursion 
      )))

(defun eval-hash-template (hash-template)
  (let ((exec-str (gethash "exec" hash-template)))
    (eval (read exec-str))))

(defun aig-post-command-hook-handle ()
  (let* ((scanned-contexts (aig-scan-context list-context-hash-templates))
         (contexts-len (length scanned-contexts)))

    ;;handles if there was a match
    (cond
     ((zerop contexts-len) nil) ;;no matches
     ((= 1 contexts-len) (eval-hash-template (car scanned-contexts))) ;;single match, execute it
     ;TODO: make the prompting
     (t (message "Multiple matches found:")) ;;more than one match, prompt for which one to use
     ))

  ;;update the context after the scanning of satisfied hooks is complete
  (aig-update-context))

;(char-to-string 10)
;(read-event)

(add-hook 'post-command-hook #'aig-post-command-hook-handle)
(remove-hook 'post-command-hook #'aig-post-command-hook-handle)

(load-templates-from-root-dir "/home/damian/bin/ELisp_files/autoinsertion_guru/")
