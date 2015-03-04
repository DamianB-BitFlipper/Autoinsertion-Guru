
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
                         (aig-load-templates)
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
    (define-key map (kbd "C-c a n") #'aig-new-template)
    (define-key map (kbd "C-c a v") #'aig-visit-template-file)
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
    ["Visit template..." aig-visit-template-file
     :help "Open a loaded template file for editing"]
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
    ["Load templates..." aig-load-directory
     :help "Load templates from a specific directory"]
    ["Reload everything" aig-load-templates
     :help "Cleanup stuff, reloads all templates"]
    ["About" aig-about
     :help "Display some information about Autoinsertion Guru"]))
;;
;; Menu details
;;

;;
;; Template Buffer Mode
;;

(defvar aig--template-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a l") #'aig-load-template-buffer)
    map)
  "The keymap used when `aig--template-mode-map' is active.")

;;;###autoload
(define-minor-mode aig--template-mode
  "Toggles the special editing mode for Auto-insertion Guru templates."
  nil
  ;;The indicator for the mode line
  " aig-template"
  :group 'autoinsertion-guru
  :keymap 'aig--template-mode-map ;;Define the keys that go with this mode

  ;;Disable any effects of this mode in non-template buffers
  ;; aig--current-buffer-template-buffer is set manually internally
  ;; if it is not set and the template-mode is enabled, that must mean that
  ;; the user turned the mode on manually, do disable it automatically
  ;; else do nothing
  (if (and aig--template-mode (not aig--current-buffer-template-buffer))
      (aig--template-mode -1) ;;disable the mode
    nil))

;;
;; Template Buffer Mode
;;

;;Provide that this file was loaded
(provide 'autoinsertion-guru-minor-mode)
