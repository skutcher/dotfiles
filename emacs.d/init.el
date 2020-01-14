;;; initfile --- Summary:
;;; Commentary:
;; Emacs 25.1 and newer tested

;; config based on
;; https://github.com/zamansky/using-emacs/blob/master/myinit.org
;;https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;;https://github.com/netromdk/.emacs.d/blob/master/init.el
;;https://github.com/nilsdeppe/MyEnvironment/blob/master/.emacs.el
;; elpa gpg keys are out of date in centos8
;; gpg2 --keyserver hkp://pool.sks-keyservers.net:80 --homedir
;;$HOME/.emacs.d/elpa/gnupg --recv-keys 066DAFCB81E42C40
;;open Emacs M+x -> package-refresh-contents

;;;my config
;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto"://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto"://stable.melpa.org/packages/")) t)
  ;;(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto"://elpa.gnu.org/packages/")) t));;)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (lsp-ivy lsp-ui company-lsp lsp-mode flycheck-pycheckers doom-modeline window-numbering use-package rainbow-delimiters flycheck esup doom-themes counsel company beacon autopair async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 512MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 512000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; show line numbers in all buffers
;;(global-display-line-numbers-mode t)
;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)

(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Auto-wrap at 80 characters
;;(setq-default auto-fill-function 'do-auto-fill)
;;(setq-default fill-column 80)
;;(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
;;(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Global Keyboard Shortcuts
;; Set help to C-?
(global-set-key (kbd "C-?") 'help-command)
;; Set mark paragraph to M-?
;;(global-set-key (kbd "M-?") 'mark-paragraph)
;; Set backspace to C-h
;;(global-set-key (kbd "C-h") 'delete-backward-char)
;; Set backspace word to M-h
;;(global-set-key (kbd "M-h") 'backward-kill-word)
;; Use meta+tab word completion
;;(global-set-key (kbd "M-TAB") 'dabbrev-expand)
;; Easy undo key
;;(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
;;(global-set-key (kbd "C-c;") 'comment-or-uncomment-region)
;; Indent after a newline, if required by syntax of language
;;(global-set-key (kbd "C-m") 'newline-and-indent)
;; Load the compile ocmmand
;;(global-set-key (kbd "C-c C-c") 'compile)
;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)
;; Don't ring the bell
(setq ring-bell-function 'ignore)
;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)
;; Disable the menu bar since we don't use it, especially not in the
;; terminal
;;(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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
;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beautifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically at closing brace, bracket and quote
(use-package autopair
  :ensure t
  :diminish autopair-mode
  :init
  :config
  (autopair-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  :config
  (beacon-mode t))

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
;; DOOM ONE Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
    :ensure t
    :config (load-theme 'doom-one t))

;; Once for every computer, the `all-the-icons-install-fonts` function
;;must be run to install fonts
;; needed by the modeline.
(use-package doom-modeline
  :requires window-numbering
  :hook (after-init . doom-modeline-mode)
  :config
  (window-numbering-mode t)

  (setq doom-modeline-minor-modes nil
        doom-modeline-enable-word-count t
        doom-modeline-checker-simple-format t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-env-python-executable "python"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END Beautifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package hydra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands (ivy-mode)
  :config
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
  :bind (("C-s" . swiper-isearch)
         )
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
         ;;("C-s" . buffer-dependent-swiper)
         ;;("C-r" . buffer-dependent-swiper)
         ("C-c g" . counsel-git-grep)
         ("C-c j" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-c d" . 'counsel-descbinds)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company - Modular in-buffer completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package company
;;  :config
;;  (setq company-idle-delay 0.5)
;;  (global-company-mode 1)
;;  (global-set-key (kbd "C-<tab>") 'company-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :diminish company-mode
  :config
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0.3)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck with LSP mode
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer t
  :init
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  ;;(when (not (display-graphic-p))
  ;;  (setq flycheck-indication-mode nil))
  )
;;(use-package flycheck-pyflakes
;;  :ensure t
;;  :after python)
;; Requires local dependencies:
;;   pip install flake8 bandit
(use-package flycheck-pycheckers
  :config
  (setq flycheck-pycheckers-checkers '(flake8 bandit)
        ;;flycheck-pycheckers-ignore-codes
        ;;'("C0411" "C0413" "C0103" "C0111" "W0142" "W0201" "W0232" "W0403" "W0511" "E1002" "E1101"
        ;;  "E1103" "R0201" "R0801" "R0903" "R0904" "R0914" "W503" "W504"
        ;;  ;; flake8
        ;;  "E111" "E114" "E121" "E126" "E127" "E221" "E241" "E302" "E305"
        ;;  ;; bandit
        ;;  "B101" "B322")
        flycheck-pycheckers-max-line-length 80
        flycheck-pycheckers-multi-thread "true")
 (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)

  ;; Ensure that the correct python checker is chosen.
  (add-hook 'python-mode-hook (lambda () (flycheck-select-checker 'python-pycheckers))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp - the language server protocol
;; writeBanner "LLVM / Clang"
;; checkProgram clang-format
;; checkProgram clangd # LSP
;;
;; writeBanner "Python"
;; checkProgram pip3
;; pip3 install python-language-server flake8 bandit
;;
;; writeBanner "Rust"
;; checkProgram rustup
;; checkProgram rustc
;; checkProgram cargo
;; rustup component add rustfmt rls rust-analysis rust-src
;;:config
;; `-background-index' requires clangd v8+!
;;  (setq lsp-clients-clangd-args '("-j=4" "-background-index""-log=error"))
;;Clangd tries to locate the “compile_commands.json” file in the root of
;;the project, so it’s useful to make a symlink in the project root and to
;;where it’s located in a build folder. Most build tools can output
;;“compile_commands.json”. In CMake you write:
;;set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
;;usefull commands:
;;lsp-find-definition
;;lsp-find-references
;;lsp-describe-thing-at-point: displays the full documentation of the thing at point
;;lsp-rename: renames the symbol (and all references to it) under point to a new name
;;lsp-execute-code-action: executes code action, like “did you mean X instead of Y?”
;;lsp-describe-session: describes current LSP session and its capabilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :init
  (use-package company-lsp
     :requires company
     :config
     (push 'company-lsp company-backends)

     ;; Disable client-side cache because the LSP server does a better job.
     (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  )
  (use-package lsp-ui
     :requires lsp-mode flycheck
     :config

     (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

     ;; Remap keys for xref find defs to use the LSP UI peek mode.
     ;;(define-key lsp-ui-mode-map [remap xref-find-definitions]#'lsp-ui-peek-find-definitions)
     ;;(define-key lsp-ui-mode-map [remap xref-find-references]#'lsp-ui-peek-find-references)

     (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )
  (use-package lsp-ivy
     :ensure t
     :diminish
     :after (lsp-mode ivy-mode))
  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp))

(provide 'init)
;;; init.el ends here
