
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

;;
;; User customizable variables
;;

;;
;; Global variables
;;
(defvar hash-templates-by-mode (make-hash-table :test #'equal)
  "A hash table that holds a list of hash tables of loaded 
templates for each respective mode identified by the key")
;;
;; Global variables
;;

;;Read the contents of a file as a list of string where each string is a line of the file
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun parse-template-file (filePath)
  (let ((contents (read-lines filePath))
        (hash-table (make-hash-table :test #'equal)))
    (parse-template-file-header contents hash-table)))

;;Parses the header of a template file and saves the parsed values in the hash-table
;; Handles errors as needed
(defun parse-template-file-header (contents hash-table)
  (if (null contents) ;;if contents is null, that means there was no #-- end identifier
      (error "Missing end identifier to template file header")
    (let ((first (car contents))
          (rest (cdr contents)))
      
      ;;Error if the regex does not match anything, that means the header was poorly formed
      (cond 
          ((string-match "^#\\s-*\\([[:alpha:]]*\\)\\s-*\\[\\(.*\\)\\]\\s-*$" first) ;;key value pair matching
           ;;Add entry to the hash-table and recurse
           (progn
             ;;The first match is the key and the second match is the value
             (puthash (match-string 1 first) (match-string 2 first) hash-table)
             (parse-template-file-header rest hash-table)))
          ((string-match "^#\\s-*--\\s-*$" first) ;;end identifier matching
           hash-table) ;;Return the hash table when done
          (t ;;Else, there must be an error
           (error "Invalid syntax on line: \"%s\"" first))))))

;;Loads all template files
(defun load-template-files ()
  ;;Clear the hash table every time the load function is called
  (clrhash hash-templates-by-mode)

  ;;TODO: make this the way it is actually supposed to be
  ;; Get what is in c-mode, defaulting to '() if there is nothing
  ;; and append the parsed data to that list back into the hash table
  (let ((mode-contents (gethash "c-mode" hash-templates-by-mode '())))
    (puthash "c-mode" 
             (cons (parse-template-file "~/bin/ELisp_files/autoinsertion_guru/example_template") mode-contents)
             hash-templates-by-mode)))

(defun aig-get-search-region (start-expr)
  (interactive "sExpr: ") ;;ask for an regexpr to look back on

  ;;make it a special let because point moves around when calling re-search-backward
  (let* ((end-point (point))
         (start-point (if (re-search-backward (concat "\\(" start-expr "\\)") nil t) ;;non-nil means matched
                          (match-end 1) ;;The end of the match
                        ;;No match means that everything before the point was exhausted, 
                        ;; so use the beginning of the file
                        1)))
    ;;move point to its original position
    (goto-char end-point)

    ;;search-area is everything back up to but not including the start-expr
    (let ((search-area (buffer-substring start-point end-point)))
      (message "%s" (string-match ".*[[:digit:]]+$" search-area)))))

(message "%s" (parse-template-file "~/bin/ELisp_files/autoinsertion_guru/example_template"))
(load-template-files)
(gethash "name" (car (gethash "c-mode" hash-templates-by-mode)))
