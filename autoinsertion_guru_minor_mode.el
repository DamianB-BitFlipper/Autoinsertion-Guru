
;;
;; Mode details
;;

;;;###autoload
(define-minor-mode aig-minor-mode
  "Toggle Auto-insertion Guru mode

When Auto-insertion Guru is enabled, it listens and tries to evaluate
hooks defined in the templates of the currently loaded major-mode."
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
  "List of functions which if evaluated returns a non-nil result, suppresses the activation of 
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

(defvar aig-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-can" #'aig-new-template)
    (define-key map "\C-cal" #'aig-load-template-buffer)
    map)
  "The keymap used when `aig-minor-mode' is active.")

;;
;; Mode details
;;

;;
;; Menu details
;;

(defvar aig--minor-mode-menu nil
  "Holds the Autoinsertion Guru menu.")

(easy-menu-define aig--minor-mode-menu
      aig-minor-mode-map
      "Menu used when `aig-minor-mode' is active."
  '("AI-Guru" :visible t
    "----"
    ["New template..." aig-new-template
     :help "Create a new template in an appropriate directory"]
    ["Load template..." aig-load-template-buffer
     :help "Loads a created template buffer temporarily into a selected mode"]
    "----"
    ("Prompting method"
     ["Ido" (setq aig-prompt-functions
                  (cons 'aig-ido-prompt
                        (remove 'aig-ido-prompt
                                aig-prompt-functions)))
      :help "Use an ido-style minibuffer prompt"
      :active t :style radio   :selected (eq (car aig-prompt-functions)
                                             'aig-ido-prompt)]
     ["Completing read" (setq aig-prompt-functions
                              (cons 'aig-completing-prompt
                                    (remove 'aig-completing-prompt
                                            aig-prompt-functions)))
      :help "Use a normal minibuffer prompt"
      :active t :style radio   :selected (eq (car aig-prompt-functions)
                                             'aig-completing-prompt)]
     )    
    "----"
    ["Load snippets..." aig-load-directory
     :help "Load snippets from a specific directory"]
    ["Reload everything" aig-load-templates
     :help "Cleanup stuff, reloads all templates"]
    ["About" aig-about
     :help "Display some information about Autoinsertion Guru"]))
;;
;; Menu details
;;

;;Provide that this file was loaded
(provide 'autoinsertion-guru-minor-mode)
