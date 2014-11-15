;;; startup.el --- after package init file

;;; Commentary:
;; My emacs configuration files. This is run in after-init-hook

;;; Code:

;; use SRGB colorspace
;; (setq ns-use-srgb-colorspace t)

;; load my color theme
;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'hc-zenburn)
;; (load-theme 'cyberpunk t)
;; (require 'powerline)
;; (require 'moe-theme)
;; (setq moe-theme-resize-markdown-title nil)
;; (setq moe-theme-resize-org-title nil)
;; (moe-theme-set-color 'orange)
;; (moe-dark)
;; (moe-light)

;; Resize titles
;; (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
;; (setq moe-theme-resize-org-title '(1.2 1.2 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
;; (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))

;; (powerline-moe-theme)
;; (powerline-default-theme)

;; (load-theme 'grandshell t)
(load-theme 'monokai t)
(set-face-attribute 'region nil :background "#00a0a0")
(add-hook 'focus-in-hook (lambda () (set-face-attribute 'region nil :background "#00a0a0")))

;; (load-theme 'sanityinc-tomorrow-day)
;; M-x ido mode
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)

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
;;           (function (lambda () (sql-highlight-postgres-keywords))))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline t)

;; enable desktop mode
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-save t)

;; (setq desktop-path '("~/.emacs.d"))
(setq desktop-dirname "~/.emacs.d")
(setq desktop-base-file-name "emacs-desktop")
(desktop-read)

;; ;; Save desktop when exiting from emacs without asking for it
;; (defun my-desktop-save ()
;; 	"Save the current desktop when exiting Emacs."
;;   (interactive)
;;   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;;   (if (eq (desktop-owner) (emacs-pid))
;;       (desktop-save desktop-dirname)))
;; (add-hook 'auto-save-hook 'my-desktop-save)

;; Change the face to something smaller
(set-face-attribute 'default nil :font "Menlo-11")

;; provides line number in the sink
(global-linum-mode 1)
(set-variable 'linum-format " %4d ")

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

(defvar tramp-persistency-file-name "~/.emacs.d/tramp")

;; Setup mac keyboard to my liking
(defvar mac-option-key-is-meta t)
(defvar mac-pass-command-to-system t)
(defvar mac-pass-control-to-system t)
(setq mac-command-modifier 'meta
      mac-option-modifier nil)

;; delete tab, not converting to space
(defvar backward-delete-char-untabify 'nil)

;; Dired configuration
;; Load Dired X when Dired is loaded.
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))

(require 'dired-single)

(setq dired-use-ls-dired t)

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

;; Use "y or n" for answers instead of complete words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-preserve-screen-position 1)           ; Scroll without moving cursor

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
;; (defadvice switch-to-buffer (before save-buffer-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice other-window (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-up (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-down (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-left (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))
;; (defadvice windmove-right (before other-window-now activate)
;;   (when buffer-file-name (save-buffer)))

;; Save all buffers
(defun save-all ()
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

;; comment out a region with this keys
;; (global-set-key (kbd "C-c C-d") 'comment-region) 

;; when a file is draged to the frame, open it
(define-key global-map [ns-drag-file] 'ns-find-file)
;; hide emacs whith cmd-h
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;; hide others with cmd-alt-h
(global-set-key (kbd "M-A-h") 'ns-do-hide-others)

;; call dash CTRL-C d
(global-set-key (kbd "C-c d") 'dash-at-point)

;; auto-complete config
(defvar ac-sources)
(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; autocomplete config
;; (require 'auto-complete-config)
;; (ac-config-default)

;; (require 'auto-complete-c-headers)
;; (add-to-list 'ac-sources 'ac-source-c-headers)

;; add new lines on end of buffer with C-n
;; (setq next-line-add-newlines t)

;; change exec path to use /usr/local/bin before everything
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin") exec-path))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (defvar ls-lisp-use-insert-directory-program nil)
)

;; display the function definition
(which-function-mode)
(defvar which-func-unknown "n/a") ;; display n/a instead of XXX
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(setq system-uses-terminfo nil)

;; (require 'ido)
;; (ido-mode nil)

(defun comment-or-uncomment-region-or-line ()
  "Like 'comment-or-uncomment-region', but if there's no mark \(that means no region\) apply comment-or-uncomment to the current line."
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))

;;(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)

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
(set-face-background 'hl-line "#2D2D2D")

;; enable flycheck for all buffers
(global-flycheck-mode 1)

;; turn off toolbar
(tool-bar-mode -1)

;; Helm configuration
(load "~/.emacs.d/heml.el")

;; (global-set-key (kbd "C-c h") 'helm-mini)
;; (require 'helm-config)
;; (helm-mode 1)

(setq inhibit-splash-screen t)

;; ssh config mode
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; enable projectile globaly
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; enable smartparens
(require 'smartparens-config)
(smartparens-global-mode)

;; enable rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; org-mode configuration
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)

(add-hook 'org-mode-hook
	  (lambda ()
	    (set-fill-column 120)
	    (auto-fill-mode)))

;; move between buffers using meta and arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'windmove)
(windmove-default-keybindings 'meta)
(setq org-replace-disputed-keys t)

(defvar windmove-wrap-around t)
;; Make windmove work in org-mode:
;; (add-hook 'org-metatup-final-hook 'windmove-up)
;; (add-hook 'org-metaleft-final-hook 'windmove-left)
;; (add-hook 'org-metadown-final-hook 'windmove-down)
;; (add-hook 'org-metaright-final-hook 'windmove-right)

;; Python programming
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Agressive indentation mode
;; (global-aggressive-indent-mode 1)

;; Company-mode
(global-company-mode 1)
(global-set-key (kbd "C-.") 'company-complete)

;; Resizes frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;; Visual regex configuration
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Magit main key
(define-key global-map (kbd "C-c m") 'magit-status)

(provide 'startup)
;;; startup.el ends here
