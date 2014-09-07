;;; package --- my init configuration
;;; Commentary:
;; my Emacs configuration
;;; Code:
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/startup.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" default)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(ensure-package-installed 'hc-zenburn-theme
			  'magit
			  'helm 'helm-flycheck 'heml-projectile
			  'flycheck
			  'dired-single
			  'auto-complete 'auto-complete-c-headers 'ac-c-headers 'ac-helm
			  'cyberpunk-theme 'dash 'epl 'git-commit-mode 'git-rebase-mode
			  'gitconfig-mode 'go-eldoc 'go-mode 'go-projectile 'grandshell-theme
			  'hc-zenburn-theme 'jinja2-mode 'magit-tramp 'moe-theme 'monokai-theme
			  'pkg-info 'rainbow-delimiters 's 'smartparens 'smex 'ssh-config-mode
			  'web-mode 'yaml-mode
			  'projectile 'helm-projectile) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)

(provide 'init)
;;; init.el ends here
