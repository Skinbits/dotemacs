;;; package --- my init configuration
;;; Commentary:
;; my Emacs configuration
;;; Code:

;; My info
(setq user-full-name "Pedro Semeano")

;; Highlight tabulations
(setq-default highlight-tabs t)

;; Show trailing white spaces
(setq-default show-trailing-whitespace nil)

;; Package init
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; This will load startup.el after all packages are initialized
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/startup.el")))

;; activate installed packages
(package-initialize)

;; Check if all the packages I use are installed. Check variable package-activated-list.
(defvar my-package-list '(2048-game ac-c-headers ac-helm popup auto-complete popup helm async ag s dash aggressive-indent names ample-theme anti-zenburn-theme auto-complete-c-headers auto-complete popup bookmark+ color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized color-theme company-anaconda anaconda-mode f dash s dash json-rpc company company-c-headers company company-go company company-inf-ruby inf-ruby company cyberpunk-theme debbugs dired+ dired-single f dash s fish-mode flymake-yaml flymake-easy gitconfig-mode go-projectile go-eldoc go-mode go-mode projectile pkg-info epl dash s grandshell-theme hc-zenburn-theme helm-flycheck helm async flycheck pkg-info epl dash dash helm-projectile projectile pkg-info epl dash s helm async helm-projectile-all s dash projectile pkg-info epl dash s helm async inf-ruby jedi python-environment deferred auto-complete popup epc ctable concurrent deferred jinja2-mode json-rpc magit-tramp magit git-rebase-mode git-commit-mode markdown-mode moe-theme monokai-theme multiple-cursors names nyan-mode org org-bullets popup powerline projectile pkg-info epl dash s python-environment deferred rainbow-delimiters s sass-mode haml-mode smartparens dash smex ssh-config-mode switch-window visual-regexp web-mode yaml-mode zenburn-theme robe))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; done

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
 '(background-color "#002b36")
 '(background-mode dark)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-mode t nil (cua-base))
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "72407995e2f9932fda3347e44e8c3f29879c5ed88da71f06ba4887b0596959a4" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4217c670c803e8a831797ccf51c7e6f3a9e102cb9345e3662cc449f4c194ed7d" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "b7d8113de2f7d9a3cf42335d8eed8415b5a417e7f6382e59076f9f4ae4fa4cee" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e194b1010c026b1401146e24a85e4b7c545276845fc38b8c4b371c8338172ad" "8f7e1668dd3a097964e6016c26d36822ab2e48fc3e9a3a2e2634224a5ca728c8" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" default)))
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
 '(org-agenda-files (quote ("~/Projects/pets.org")))
 '(org-hide-leading-stars nil)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
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

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(provide 'init)
;;; init.el ends here


