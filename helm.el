;;; helm-local-config.el --- Local configuration for helm

;;; Commentary:
;; Local configuration for helm from http://tuhdo.github.io/helm-intro.html

;;; Code:
;; Install helm packages. Can't use use-package because I need several loaded
(require 'helm)

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(defvar helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
(global-set-key (kbd "M-x") 'helm-M-x)

;; (setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(setq
  helm-google-suggest-use-curl-p t
  helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
  helm-quick-update t ; do not display invisible candidates
  helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
  helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

  helm-split-window-default-side 'other ;; open helm buffer in another window
  helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
  helm-buffers-favorite-modes (append helm-buffers-favorite-modes
				      '(picture-mode artist-mode))
  helm-candidate-number-limit 200 ; limit the number of displayed canidates
  helm-M-x-requires-pattern 0     ; show all candidates when set to 0
  helm-boring-file-regexp-list
  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
  helm-ff-file-name-history-use-recentf t
  helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
  ido-use-virtual-buffers t      ; Needed in helm-buffers-list
  helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
 )


;; ac-helm config
(require 'ac-helm)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)


(setq helm-autoresize-min-height 40)
(setq helm-autoresize-min-height 40)

(helm-autoresize-mode t) ;; no like it

(require 'projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)

;; force projectile to use helm
(setq projectile-completion-system 'helm)
;; (helm-projectile-on)

(helm-mode 1)

(message "helm loaded")
(provide 'helm)
;;; helm.el ends here
