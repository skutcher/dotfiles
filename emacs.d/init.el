(defvar my:use-theme "doom-one")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(
			 ;;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;;("gnu" . "http://elpa.gnu.org/packages/")
			 ))
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse
           (apply #'nconc
                  ;; Only keep package.el provided loadpaths.
                  (mapcar #'(lambda (path)
                              (if (string-prefix-p package-user-dir-real path)
                                  (list path)
                                nil))
                          load-path))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 256MB of memory,  
;; we increase it during initialization.
(setq gc-cons-threshold 256000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; Extra plugins and config files are stored here
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one 
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)                                                                                                                   
;; Show column numbers by default                                                                                                           
(setq column-number-mode t)
;; Prevent emacs from creating a bckup file filename~                                                                                       
(setq make-backup-files nil)
;; Settings for searching                                                                                                                   
(setq-default case-fold-search t ;case insensitive searches by default                                                                      
              search-highlight t) ;hilit matches when searching                                                                             
;; Highlight the line we are currently on                                                                                                   
(global-hl-line-mode t)
;; Disable the toolbar at the top since it's useless                                                                                        
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
;; Remove trailing white space upon saving                                                                                                  
;; Note: because of a bug in EIN we only delete trailing whitespace                                                                         
;; when not in EIN mode.                                                                                                                    
(add-hook 'before-save-hook
          (delete-trailing-whitespace))
;; Auto-wrap at 80 characters                                                                                                               
(setq-default auto-fill-function 'do-auto-fill)                                                                                             
(setq-default fill-column 80)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode                                                                                               
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; We don't want to type yes and no all the time so, do y and n                                                                             
(defalias 'yes-or-no-p 'y-or-n-p)                                                                                                           
;; Disable the horrid auto-save                                                                                                             
(setq auto-save-default nil)
;; Disable menu bar in non grpahic mode
(if (display-graphic-p)
  (menu-bar-mode 1)
  (menu-bar-mode -1)
  )

;; Don't ring the bell                                                                                                                      
(setq ring-bell-function 'ignore)                                                                                                           

;; Non-nil means draw block cursor as wide as the glyph under it.                                                                           
;; For example, if a block cursor is over a tab, it will be drawn as                                                                        
;; wide as that tab on the display.                                                                                                        
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git                                                                                                        
(setq vc-follow-symlinks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; async - library for async/thread processing                                                                                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package async
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (async use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; auto-package-update                                                                                                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Auto update packages once a week                                                                                                         
(use-package auto-package-update
  :ensure t                                                                                                                                 
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; diminish - Hide the minor modes in the mode line for more room                                                                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package diminish
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings                                                                                                    
    (declare-function diminish "diminish.el"))
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Appearance                                                                                                                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string-equal my:use-theme "doom-one")
  (use-package doom-themes
               :ensure t
               :config (load-theme 'doom-one t)))
