;;; package --- my init configuration
;;; Commentary:
;; my Emacs configuration
;;; Code:
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/startup.el")))

(toggle-frame-maximized)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e194b1010c026b1401146e24a85e4b7c545276845fc38b8c4b371c8338172ad" "8f7e1668dd3a097964e6016c26d36822ab2e48fc3e9a3a2e2634224a5ca728c8" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" default)))
 '(diredp-hide-details-initially-flag nil)
 '(fci-rule-color "#49483E")
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
 '(magit-diff-use-overlays nil)
 '(nyan-mode nil)
 '(org-agenda-files (quote ("~/Projects/pets.org")))
 '(org-hide-leading-stars nil)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
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
 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Inconsolata"))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1.0))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 1.0)))))

;; install needed packages automatcly
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed PACKAGES or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; activate installed packages
(package-initialize)

(ensure-package-installed 'hc-zenburn-theme
			  'magit
			  'helm 'helm-flycheck 'helm-projectile
			  'flycheck
			  'dired-single
			  'auto-complete 'auto-complete-c-headers 'ac-c-headers 'ac-helm
			  'cyberpunk-theme 'dash 'epl 'git-commit-mode 'git-rebase-mode
			  'gitconfig-mode 'go-eldoc 'go-mode 'go-projectile 'grandshell-theme
			  'hc-zenburn-theme 'jinja2-mode 'magit-tramp 'moe-theme 'monokai-theme
			  'pkg-info 'rainbow-delimiters 's 'smartparens 'smex 'ssh-config-mode
			  'web-mode 'yaml-mode
			  'projectile 'helm-projectile
			  'ag 'sass-mode) ;  --> (nil nil) if iedit and magit are already installed

(setq line-spacing 0.06)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
