(defvar my:use-theme "doom-one")
;; (add-to-list 'term-file-aliases
;;     '("xterm-termite" . "rxvt-unicode-256color"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(
    ;;("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")
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
(setq frame-background-mode 'dark)
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
;; esup: Emacs StartUp Profiler                                                                                                             
;;       - Profile the load time of the Emacs init file                                                                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package esup
  :ensure t
  :init
  (setq esup-child-max-depth 0)
  ;; Use a hook so the message doesn't get clobbered by other messages.                                                                     
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (message "Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable terminal emacs to copy and paste from system clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this uses C-c before C-w, M-w, and M-y
;; From: https://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(defun my-copy-to-xclipboard(arg)
  "Copy the selection ARG to the X11 clipboard."
  (interactive "P")
  (cond
   ((not (use-region-p))
    (message "Nothing to yank to X-clipboard"))
   ((and (not (display-graphic-p))
	 (/= 0 (shell-command-on-region
		(region-beginning) (region-end) "xsel -i -b")))
    (message "Error: Is program `xsel' installed?"))
   (t
    (when (display-graphic-p)
      (call-interactively 'clipboard-kill-ring-save))
    (message "Yanked region to X-clipboard")
    (when arg
      (kill-region  (region-beginning) (region-end)))
    (deactivate-mark))))

(defun my-cut-to-xclipboard()
  "Cut the selection to the X11 clipboard."
  (interactive)
  (my-copy-to-xclipboard t))

(defun my-paste-from-xclipboard()
  "Paste the selection from the X11 clipboard."
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (shell-command-to-string "xsel -o -b"))))

(global-set-key (kbd "C-c C-w") 'my-cut-to-xclipboard)
(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
;; Use C-c M-y instead of C-c C-y so it works in Python mode too.
(global-set-key (kbd "C-c M-y") 'my-paste-from-xclipboard)

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
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands (ivy-mode)
  :config
  ;;(when my:byte-compile-init
  ;;  (require 'ivy))
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  )

(use-package swiper
  :ensure t
  )

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("C-c i" . counsel-info-lookup-symbol)
         ("C-c u" . counsel-unicode-char)
         ("C-s" . buffer-dependent-swiper)
         ("C-r" . buffer-dependent-swiper)
         ("C-c g" . counsel-git-grep)
         ("C-c j" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         )
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s .")
    (warn "\nWARNING: Could not find the ripgrep executable. It "
          "is recommended you install ripgrep."))

  ;; Switch whether we use swiper or counsel-grep depending on the major mode.
  ;; This is because for certain themes font highlighting is very expensive
  ;; in some modes (e.g. C++ mode)
  (defun buffer-dependent-swiper (&optional initial-input)
    (interactive)
    (if (or (not buffer-file-name)
            (ignore-errors
              (file-remote-p (buffer-file-name)))
            (if (or (eq major-mode 'org-mode)
                    (eq major-mode 'c++-mode))
                (<= (buffer-size) 50000)
              ;; The value 300000 is the default number of characters
              ;; before falling back to counsel-grep from swiper.
              (<= (buffer-size) 300000)))
        (swiper initial-input)
      (progn
        (when (file-writable-p buffer-file-name)
          (save-buffer))
        (counsel-grep initial-input))))
  )

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
    (declare-function counsel-etags-guess-program "counsel-etags.el")
    (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
  :bind (
         ("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  (add-hook
   'prog-mode-hook
   (lambda () (add-hook 'after-save-hook
                        (lambda ()
                          (counsel-etags-virtual-update-tags)))))

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  ;; (defun my-scan-dir (src-dir &optional force)
  ;;   "Create tags file from SRC-DIR. \
  ;;    If FORCE is t, the commmand is executed without \
  ;;    checking the timer."
  ;;   (let* ((find-pg (or
  ;;                    counsel-etags-find-program
  ;;                    (counsel-etags-guess-program "find")))
  ;;          (ctags-pg (or
  ;;                     counsel-etags-tags-program
  ;;                     (format "%s -e -L" (counsel-etags-guess-program
  ;;                                         "ctags"))))
  ;;          (default-directory src-dir)
  ;;          ;; run find&ctags to create TAGS
  ;;          (cmd (format
  ;;                "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
  ;;                find-pg
  ;;                (mapconcat
  ;;                 (lambda (p)
  ;;                   (format "-iwholename \"*%s*\"" p))
  ;;                 counsel-etags-ignore-directories " -or ")
  ;;                counsel-etags-max-file-size
  ;;                (mapconcat (lambda (n)
  ;;                             (format "-not -name \"%s\"" n))
  ;;                           counsel-etags-ignore-filenames " ")
  ;;                ctags-pg))
  ;;          (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
  ;;          (doit (or force (not (file-exists-p tags-file)))))
  ;;     ;; always update cli options
  ;;     (when doit
  ;;       (message "%s at %s" cmd default-directory)
  ;;       (async-shell-command cmd)
  ;;       (visit-tags-table tags-file t))))

  ;; (setq counsel-etags-update-tags-backend
  ;;       (lambda ()
  ;;         (interactive)
  ;;         (let* ((tags-file (counsel-etags-locate-tags-file)))
  ;;           (when tags-file
  ;;             (my-scan-dir (file-name-directory tags-file) t)
  ;;             (run-hook-with-args
  ;;              'counsel-etags-after-update-tags-hook tags-file)
  ;;             (unless counsel-etags-quiet-when-updating-tags
  ;;               (message "%s is updated!" tags-file))))))
  )

;; Set up projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :defer 1
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function projectile-mode "projectile.el"))
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package counsel-projectile
  :ensure t
  :after projectile
  :bind (("C-x M-f" . counsel-projectile-find-file))
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function counsel-projectile-mode "counsel-projectile.el"))
  :config
  (counsel-projectile-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                     
;; Window numbering                                                                                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Package window-numbering installed from package list                                                                                     
;; Allows switching between buffers using meta-(# key)                                                                                      
(use-package window-numbering
  :ensure t
  :config
  (eval-when-compile
    ;; Silence missing function warnings                                                                                                    
    (declare-function window-numbering-mode "window-numbering.el"))
  (window-numbering-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Rainbow Delimiters -  have delimiters be colored by their depth                                                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings                                                                                                    
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Beacon-mode: flash the cursor when switching buffers or scrolling                                                                        
;;              the goal is to make it easy to find the cursor                                                                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings                                                                                                    
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; which-key: when you pause on a keyboard shortcut it provides                                                                             
;;            suggestions in a popup buffer                                                                                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings                                                                                                    
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;; We want to be able to see if there is a tab character vs a space.                                                                        
;; global-whitespace-mode allows us to do just that.                                                                                        
;; Set whitespace mode to only show tabs, not newlines/spaces.                                                                              
(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :init
  (eval-when-compile
      ;; Silence missing function warnings                                                                                                  
      (declare-function global-whitespace-mode "whitespace.el"))
  :config
  (setq whitespace-style '(lines-tail trailing tabs tab-mark))
  ;; Turn on whitespace mode globally.                                                                                                      
  (global-whitespace-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: lsp (language server protocol mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A code completion, syntax checker, etc. engine that uses the LSP to
;; talk to completion servers.
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (;; C++ completers are: ccls, clangd, or cquery. I use clangd.
         (c-mode-common . lsp)
         ;; Python on Linux/mac OS is pyls (python language server)
         (python-mode . lsp)
         ;; Rust RLS (Rust Language Server) https://github.com/rust-lang/rls
         ;; (rust-mode . lsp)
         ;; Bash uses bash-language-server
         ;; https://github.com/mads-hartmann/bash-language-server
         ;; (shell-mode . lsp)
         )
  :init
  ;; Disable yasnippet. We re-enable when yasnippet is loaded.
  ;; (defvar lsp-enable-snippet nil)
  (use-package lsp-ui
    :ensure t
    :after lsp-mode
    :hook (lsp-mode . lsp-ui-mode)
    :config
    ;; Set useful keybindings
    (local-set-key (kbd "C-c y l") 'lsp-ui-flycheck-list)
    (local-set-key (kbd "C-c y i") 'lsp-ui-imenu)

    ;; Use find references and definitions key bindings instead of CTags.
    (defun set-local-keybinds-lsp-ui ()
      "Sets keybindings for lsp mode"
      (interactive)
      (local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
      (local-set-key (kbd "M-?") 'lsp-ui-peek-find-references)
      )
    (add-hook 'c-mode-common-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'python-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'rust-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'shell-mode-hook 'set-local-keybinds-lsp-ui)
    )

  (use-package company-lsp
    :ensure t
    :diminish
    :after (company lsp-mode)
    :init
    (defvar company-lsp-enable-recompletion t)
    (defvar company-lsp-async t)
    :config (add-to-list 'company-backends 'company-lsp))

  (if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
  (if (not (file-exists-p "~/.emacs.d/plugins/lsp-ivy.el"))
      (url-copy-file
       "https://raw.githubusercontent.com/emacs-lsp/lsp-ivy/master/lsp-ivy.el"
       "~/.emacs.d/plugins/lsp-ivy.el"))
  (if (file-exists-p "~/.emacs.d/plugins/lsp-ivy.el")
      ;;lsp-ivy is not yet on Melpa...
      (use-package lsp-ivy
	:ensure t
	:diminish
	:after (lsp-mode ivy-mode)
	) 
  )

  :config
  ;; Extra flags passed to clangd. See 'clangd --help' for info
  ;; (defvar lsp-clients-clangd-args '("--clang-tidy"
  ;;                                   "--fallback-style=google"
  ;;                                   "-j=4"
  ;;                                   "--suggest-missing-includes"
  ;;                                   "--pch-storage=memory"))
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-before-save-edits nil)
  ;; Use flycheck instead of flymake
  (setq lsp-prefer-flymake nil)

  ;; Set keybindings
  (local-set-key (kbd "C-c y n") 'lsp-rename)
  (local-set-key (kbd "C-c y o") 'lsp-restart-workspace)
  (local-set-key (kbd "C-c y c") 'lsp-disconnect)
  (local-set-key (kbd "C-c f") 'lsp-format-region)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; web-mode                                                                                                                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; autopair                                                                                                                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Automatically at closing brace, bracket and quote                                                                                        
(use-package autopair
  :ensure t
  :diminish autopair-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings                                                                                                    
    (declare-function autopair-global-mode "autopair.el"))
  :config
  (autopair-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Magit                                                                                                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package magit
  :ensure t
  :after (ivy)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :init
  (use-package dash
    :ensure t)
  :config
  (add-hook 'magit-mode-hook (lambda () (setq whitespace-mode -1)))
  (setq magit-completing-read-function 'ivy-completing-read)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; gitignore-mode: highlighting in gitignore files                                                                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package gitignore-mode
  :ensure t
  :diminish gitignore-mode
  :mode ("\\.gitignore\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; yaml-mode                                                                                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; json-mode                                                                                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.imp\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Package: lsp (language server protocol mode)                                                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; A code completion, syntax checker, etc. engine that uses the LSP to                                                                      
;; talk to completion servers.                                                                                                              
;; https://github.com/emacs-lsp/lsp-mode                                                                                                    
(use-package lsp-mode
  :ensure t                                                                                                                                 
  :hook (;; C++ completers are: ccls, clangd, or cquery. I use clangd.                                                                      
         (c-mode-common . lsp)
         ;; Python on Linux/mac OS is pyls (python language server)                                                                         
         (python-mode . lsp)                                                                                                              
         ;; Rust RLS (Rust Language Server) https://github.com/rust-lang/rls                                                                
         ;;(rust-mode . lsp)                                                                                                                
         ;; Bash uses bash-language-server                                                                                                  
         ;; https://github.com/mads-hartmann/bash-language-server                                                                           
         (shell-mode . lsp)
         )
  :init
  ;; Disable yasnippet. We re-enable when yasnippet is loaded.                                                                               
  (defvar lsp-enable-snippet nil)
  (use-package lsp-ui
    :ensure t
    :after lsp-mode
    :hook (lsp-mode . lsp-ui-mode)
    :config
    ;; Set useful keybindings                                                                                                                
    (local-set-key (kbd "C-c y l") 'lsp-ui-flycheck-list)
    (local-set-key (kbd "C-c y i") 'lsp-ui-imenu)

    ;; Use find references and definitions key bindings instead of CTags.                                                                    
    (defun set-local-keybinds-lsp-ui ()
      "Sets keybindings for lsp mode"
      (interactive)
      (local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
      (local-set-key (kbd "M-?") 'lsp-ui-peek-find-references)
      )
    (add-hook 'c-mode-common-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'python-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'rust-mode-hook 'set-local-keybinds-lsp-ui)
    (add-hook 'shell-mode-hook 'set-local-keybinds-lsp-ui)
    )
  )

(use-package lsp-treemacs
  :ensure t
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                      
;; Appearance                                                                                                                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string-equal my:use-theme "doom-one")
  (use-package doom-themes
               :ensure t
               :config (load-theme 'doom-one t)))

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              0.5
;;           treemacs-width                         35)
    
;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)
;;     (treemacs-tag-follow-mode t)
;;     ;;(treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
    
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("C-x t s"   . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)
