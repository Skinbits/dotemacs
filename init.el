;;; package --- my init configuration
;;; Commentary:
;; my Emacs configuration
;;; Code:

;; create Messages buffer

(messages-buffer)

;; My info
(setq user-full-name "Pedro Semeano")

;; Highlight tabulations
(setq-default highlight-tabs t)

;; Show trailing white spaces
(setq-default show-trailing-whitespace nil)

;; Package init
(require 'package)
;; (setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t) ;; Not working at the moment. error tryint to access host
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; activate installed packages
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; This will load startup.el after all packages are initialized
;; (add-hook 'after-init-hook (lambda () (load "~/.emacs.d/startup.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("1cd9defef2a98138c732728568b04043afd321eb802d25a254777de9b2463768" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "72407995e2f9932fda3347e44e8c3f29879c5ed88da71f06ba4887b0596959a4" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4217c670c803e8a831797ccf51c7e6f3a9e102cb9345e3662cc449f4c194ed7d" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "b7d8113de2f7d9a3cf42335d8eed8415b5a417e7f6382e59076f9f4ae4fa4cee" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e194b1010c026b1401146e24a85e4b7c545276845fc38b8c4b371c8338172ad" "8f7e1668dd3a097964e6016c26d36822ab2e48fc3e9a3a2e2634224a5ca728c8" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" default)))
 '(diredp-hide-details-initially-flag nil)
 '(fci-rule-color "#49483E")
 '(foreground-color "#839496")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(magit-diff-options (quote ("--ignore-space-change" "--minimal")))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files (quote ("~/Projects/pets.org")))
 '(org-hide-leading-stars nil)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(paradox-automatically-star nil)
 '(paradox-github-token t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 100 :width condensed :foundry "nil" :family "Source Code Pro"))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.0))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :height 1.0)))))


(setq line-spacing 0.06)
(put 'downcase-region 'disabled nil)

;; Emacs configuration
;; configure encoding system
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq buffer-file-coding-system 'utf-8)

;; maximum decoration for faces
(setq font-lock-maximum-decoration t)

;;; Set the type of SQL to postgres, the main database that I use
;; (add-hook 'sql-mode-load-hook
          ;; (function (lambda () (sql-highlight-postgres-keywords))))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline t)

;; setup bookmark system
(defvar bookmark-default-file "~/.emacs.d/bookmarks")
(defvar bookmark-save-flag 1)

;; how to do backup files. Save it in $HOME/.saves, never on same directory of file
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '((".*" . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)               ; use versioned backups

;; (setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq auto-save-interval 5000)
(setq auto-save-timeout 30)

;; TRAMP Setup
(defvar tramp-persistency-file-name "~/.emacs.d/tramp")

;; Setup mac keyboard to my liking
(defvar mac-option-key-is-meta t)
(defvar mac-pass-command-to-system t)
(defvar mac-pass-control-to-system t)
(setq mac-command-modifier 'meta
      mac-option-modifier nil)

;; delete tab, not converting to space
(defvar backward-delete-char-untabify 'nil)

;; tab modes
;; to setup tabs
(defvar c-basic-indent 2)
(setq tab-width 2)
(defvar default-tab-width 2)
(setq standard-indent 2)
(setq require-final-newline t)
(defvar c-basic-offset 2)
(setq indent-tabs-mode nil)
; (setq tab-stop-list '((4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))

(add-hook 'c-mode-hook
          '(lambda()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             )
          )

(add-hook 'cc-mode-hook
          '(lambda()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             )
          )

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (defvar python-indent 4)
	    ))

(add-hook 'fundamental-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             )
          )

(add-hook 'javascript-ide-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             )
          )

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq tab-width 2)
	    ))

;; Use "y or n" for answers instead of complete words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq scroll-preserve-screen-position 1)           ; Scroll without moving cursor

(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Save all buffers when losing focus
(defun save-all ()
  "Save all files that are needeed."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;; save history between sessions
(savehist-mode 1)
(defvar savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(defvar savehist-file "~/.emacs.d/tmp/savehist")

;; sane defaults to home and end
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; set C style editing
(if (>= emacs-major-version 20)
    (add-hook 'c-mode-hook '(lambda () (c-set-style "linux")))
  (add-hook 'c-mode-hook '(lambda () (set-c-style "linux"))))

(if (>= emacs-major-version 20)
    (add-hook 'c++-mode-hook '(lambda () (c-set-style "linux")))
  (add-hook 'c++-mode-hook '(lambda () (set-c-style "linux"))))

;; move between subwords in variables names in CamelCase
;; enable for all programming modes
(add-hook 'prog-mode-hook 'subword-mode)

;; Display with different colors the commentary for fixes and todos
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; when a file is draged to the frame, open it
(define-key global-map [ns-drag-file] 'ns-find-file)
;; hide emacs whith cmd-h
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;; hide others with cmd-alt-h
(global-set-key (kbd "M-A-h") 'ns-do-hide-others)

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

(global-set-key (kbd "C-;") #'endless/comment-line)

(setenv "GOPATH" "/Users/pedro/Projects/go/space")

;; call dash CTRL-C d
(use-package dash-at-point
  :ensure t
  :bind ("C-c d" . dash-at-point)
  :config
  (message "loading dash-at-point")
)

;; change exec path to use /usr/local/bin before everything
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin") exec-path))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (defvar ls-lisp-use-insert-directory-program nil)
)

;; display the function definition
;; (which-function-mode)
;; (defvar which-func-unknown "n/a") ;; display n/a instead of XXX
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       ;; We remove Which Function Mode from the mode line, because it's mostly
;;       ;; invisible here anyway.
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

(setq system-uses-terminfo nil)

;; "smart" home, i.e., home toggles b/w 1st non-blank character and 1st column
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or 'beginning-of-line'."
  (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#1D1D1D")

;; turn off toolbar
(tool-bar-mode -1)

(setq inhibit-splash-screen t)

;; Set the name of the frame to the name of the File with directory
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Resizes frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(global-set-key (kbd "C-x o") 'switch-window)

(winner-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ediff)
;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; When displaying flycheck error list, create a window by splitting the current window
;; with 20% size, reusable on bottom
(add-to-list 'display-buffer-alist
	     `(,(rx bos "*Flycheck errors*" eos)
	       (display-buffer-reuse-window
		display-buffer-below-selected)
	       (reusable-frames . visible)
	       (side            . bottom)
	       (window-height   . 0.2)))


;; (use-package paradox
;;   :ensure t
;;   :config
;;   (message "Loading paradox")
;;   )

;; After packages init configuration

(load "~/.emacs.d/helm.el")

;; Packages configuration
(use-package dired-x
  :config
  (message "loading dired-x")
  (setq-default dired-omit-files-p t) ; this is buffer-local variable
  (setq dired-omit-files
	(concat dired-omit-files "\\|^\\..+$"))
  (setq dired-use-ls-dired t)
  )

(use-package ibuffer
  :ensure t
  :bind (("C-x C-b" . ibuffer-other-window))
  :config
    (message "loading ibuffer")
  )

;; (use-package hc-zenburn-theme
;;   :config
;;   (message "loading hc-zenburn")
;;   (load-theme 'hc-zenburn)
;;   )

;; (use-package monokai-theme
;;   :ensure t
;;   :config (load-theme 'monokai t)
;;   ;; :init (set-frame-font "-*-DejaVu Sans Mono-light-normal-normal-*-10-*-*-*-m-0-iso10646-1"))
;;   )

(declare-function moe-dark "moe-theme.el" nil)
(use-package moe-theme
  :config
  ;; Resize titles (optional).
  (setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
  ;; (setq moe-theme-resize-org-title '(1.2 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
  ;; (setq moe-theme-resize-org-title '(1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2 1.2))
  (setq moe-theme-resize-org-title nil)
  (setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
  (moe-dark)
  )

(message "loading desktop")
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-dirname "~/.emacs.d")
(setq desktop-base-file-name "emacs-desktop")

(use-package hlinum
  :ensure t
  :config
  (message "loading hlinum")
  (hlinum-activate)
  ;; (global-linum-mode 1)		 ; global linnum mode
  (add-hook 'prog-mode-hook 'linum-mode) ; Only for programming modes.
  (set-variable 'linum-format " %4d ")
  (setq linum-highlight-in-all-buffersp t))

(use-package flycheck
  :ensure t
  :config
  ;; enable flycheck for all buffers
  (message "loading flycheck")
  (global-flycheck-mode 1)
  )

(use-package ssh-config-mode
  :ensure t
  :config
  (message "loading ssh-config-mode")
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  )

;; enable smartparens
(use-package smartparens
  :config
  (message "loading smartparens")
  (require 'smartparens-config)
  (smartparens-global-mode)
  )

(use-package rainbow-delimiters
  :config
  (message "loading rainbow-delimiters")
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;; org-mode configuration
(use-package org
  :ensure t
  :bind
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)
  :config
  (message "loading org")
  (setq org-log-done 'time)
  (setq org-replace-disputed-keys t)
  ;; When loading a org-mode file we force word warp
  (add-hook 'org-mode-hook
      (lambda ()
        (set-fill-column 80)
        (auto-fill-mode)))
  )


;; move between buffers using meta and arrow keys
(use-package windmove
  :config
  (message "loading windmove")
  (windmove-default-keybindings 'meta)
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))
  (defvar windmove-wrap-around t)
)

;; Python programming
(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  )

;;  ----------------------
(use-package anaconda-mode
  :ensure t
  :config
  (message "loading anaconda-mode")
  (add-hook 'python-mode-hook 'anaconda-mode)
  )

;; ;; Company-mode
(use-package company
  :ensure t
  :bind ("C-." . company-complete)
  :config
  (message "loading company-mode")
  (global-company-mode 1)
)

(use-package company-anaconda
  :ensure t
  :config
  (message "loading company-anaconda")
  (add-to-list 'company-backends 'company-anaconda)
  )

;; This package is great. But it interferes with helm in strange ways. Do not use.
;; (use-package perspective
;;   :ensure t
;;   :config
;;   (message "loading perspective")
;;   (persp-mode)
;;   )

;; (global-key-binding (kbd "C-.") 'company-complete)

(use-package visual-regexp
  :ensure t
  :config
  (message "loading visual-regexp")
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  )

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config
    (message "loading magit")
  )


(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq shell-file-name "/usr/local/bin/zsh")
(defvar explicit-zsh-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(global-set-key (kbd "C-c C-t") '(lambda ()
				   (interactive)
				   (ansi-term "/usr/local/bin/zsh")))

(use-package ruby-mode
  :ensure t
  :config
  (message "loading ruby-mode")
  ;; avoid deep indentation
  (setq ruby-deep-indent-paren nil)
  )

(use-package rvm
  :ensure t
  :config
    (message "loading rvm")
  )

(use-package robe
  :ensure t
  :bind
  ("C-c i" . inf-ruby)
  ("C-c a" . rvm-activate-corresponding-ruby)
  :config
  (message "loading robe")
  (add-hook 'ruby-mode-hook 'robe-mode)
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    "Activate corresponding ruby from rvm when using robe."
    (rvm-activate-corresponding-ruby))
  
  ;; add robe to company-mode
  (push 'company-robe company-backends)
  )

(use-package web-mode
  :config
  (message "loading web-mode")
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
  
  (defvar web-mode-engines-alist
    '(("php"    . "\\.phtml\\'")
      ("blade"  . "\\.blade\\."))
    )
  )

(use-package jinja2-mode
  :ensure t
  :config
  (message "loading jinja2-mode")
  )
  
;; (use-package ace-jump-mode
;;   :ensure t
;;   :bind
;;   ("C-c SPC" . ace-jump-mode)
;;   ("C-x SPC" . ace-jump-mode-pop-mark)
;;   :config
;;   (message "loading ace-jump-mode")
;;   (autoload
;;     'ace-jump-mode
;;     "ace-jump-mode"
;;     "Emacs quick move minor mode"
;;     t)
;;   (autoload
;;     'ace-jump-mode-pop-mark
;;     "ace-jump-mode"
;;     "Ace jump back:-)"
;;     t)
;;   (eval-after-load "ace-jump-mode"
;;     '(ace-jump-mode-enable-mark-sync))
;;   )

(use-package ace-window
  :ensure t
  :bind
  ("C-c w" . ace-window)
  ("C-c SPC" . avi-goto-word-1)
  :config
  (message "Loading ace-window")
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defvar avi-keys)
  (setq avi-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
  )


(use-package expand-region
  :ensure t
  :bind ("C-+" . er/expand-region)
  :config
    (message "loading expand-region")
)


(use-package key-chord
  :config
  (message "loading key-chord")
  (key-chord-define-global "FF" 'find-file)
  (key-chord-define-global "jk" 'beginning-of-buffer)
  (key-chord-define-global "jj" 'avi-goto-word-1)
  (key-chord-define-global "jl" 'ace-window)
  (key-chord-mode +1))

(use-package rust-mode
  :ensure t
  :config
  (message "loading rust-mode")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  ;; make ctrl-z undo
  (global-set-key (kbd "C-z") 'undo)
  ;; make ctrl-Z redo
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo) ;; turn on everywhere
  )
;; Change to our font
;; (set-frame-font "Inconsolata 12")
;; (set-frame-font "Menlo 11")
;; (set-frame-font "-*-Source Code Pro-light-normal-normal-*-10-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Inconsolata-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Source Code Pro-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Menlo-light-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; (set-frame-font "-*-Source Code Pro-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Menlo-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;;(set-frame-font "-*-Monaco-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Anonymous Pro-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Droid Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Ubuntu Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Source Code Pro-light-normal-normal-*-10-*-*-*-m-0-iso10646-1")
(set-frame-font "-*-Source Code Pro-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")


(load "~/.emacs.d/secrets.el")
(put 'narrow-to-region 'disabled nil)

(message "Ended init")
(provide 'init)
;;; init.el ends here
