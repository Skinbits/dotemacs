;;; setup-bookmarks.el --- Hydra setup for bookmarks
;;; Commentary:
;;;  Create Hydra for bookmarks
;;;  Original source: https://github.com/joodland/bm

;;; Code:
(require 'use-package)
(use-package bm
  :config
  (progn
    (setq-default bm-buffer-persistence t) ; buffer persistence on by default

    (when bm-buffer-persistence
      (setq bm-repository-file (locate-user-emacs-file "bm-repository"))

      ;; Load bm repository
      (when (file-exists-p bm-repository-file)
        (bm-repository-load))

      ;; Saving bookmarks
      (add-hook 'kill-buffer-hook #'bm-buffer-save)
      ;; Saving the repository to file when on exit.
      ;; kill-buffer-hook is not called when Emacs is killed, so we
      ;; must save all bookmarks first.
      (defun modi/bm-save-all-bm-to-repository ()
        (bm-buffer-save-all)
        (bm-repository-save))
      (add-hook 'kill-emacs-hook #'modi/bm-save-all-bm-to-repository)
      (add-hook 'after-save-hook #'bm-buffer-save)
      ;; The `after-save-hook' is not necessary to use to achieve persistence,
      ;; but it makes the bookmark data in repository more in sync with the file
      ;; state.

      ;; Restoring bookmarks
      (add-hook 'find-file-hooks   #'bm-buffer-restore)
      (add-hook 'after-revert-hook #'bm-buffer-restore)
      ;; The `after-revert-hook' is not necessary to use to achieve persistence,
      ;; but it makes the bookmark data in repository more in sync with the file
      ;; state. This hook might cause trouble when using packages
      ;; that automatically reverts the buffer (like vc after a check-in).
      ;; This can easily be avoided if the package provides a hook that is
      ;; called before the buffer is reverted (like `vc-before-checkin-hook').
      ;; Then new bookmarks can be saved before the buffer is reverted.
      ;; Make sure bookmarks is saved before check-in (and revert-buffer)
      (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

    (define-fringe-bitmap 'bm-marker-left [#xF8   ; ▮ ▮ ▮ ▮ ▮ 0 0 0
                                           #xFC   ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                           #xFE   ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                           #x0F   ; 0 0 0 0 ▮ ▮ ▮ ▮
                                           #x0F   ; 0 0 0 0 ▮ ▮ ▮ ▮
                                           #xFE   ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                           #xFC   ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                           #xF8]) ; ▮ ▮ ▮ ▮ ▮ 0 0 0

    (setq bm-highlight-style 'bm-highlight-only-fringe)
    (setq bm-cycle-all-buffers t) ; search all open buffers for bookmarks

    (defun modi/bm-bookmark-regexp ()
      (interactive)
      (if (use-region-p)
          (progn
            (bm-bookmark-regexp-region (region-beginning) (region-end))
            (deactivate-mark))
        (bm-bookmark-regexp)))

    (defhydra hydra-bm (:color pink
                        :hint nil
                        :body-pre (when (not (use-region-p)) (push-mark)))
      "
Bookmark _n_ext (_N_ in lifo order)            toggle book_m_ark        ^^_/_ bm lines matching regexp                          toggle per_s_istence
         _p_revious (_P_ in lifo order)        _a_nnotate               _x_/_X_ remove all bm from current/all buffer(s)        _r_eturn to from where you started
    "
      ("m"   bm-toggle)
      ("M"   bm-toggle :color blue)
      ("a"   bm-bookmark-annotate :color blue)
      ("n"   bm-common-next)
      ("N"   bm-lifo-next)
      ("p"   bm-common-previous)
      ("P"   bm-lifo-previous)
      ("/"   modi/bm-bookmark-regexp :color blue)
      ("s"   bm-toggle-buffer-persistence)
      ("x"   bm-remove-all-current-buffer :color blue)
      ("X"   bm-remove-all-all-buffers :color blue)
      ("r"   pop-to-mark-command :color blue)
      ("RET" nil "cancel" :color blue)
      ("q"   nil "cancel" :color blue))
    (bind-key "C-c b" #'hydra-bm/body modi-mode-map)

    (bind-keys ; bind left mouse clicks and scrolls in left margin/fringe
     ("<left-fringe> <mouse-5>" . bm-next-mouse)
     ("<left-margin> <mouse-5>" . bm-next-mouse)
     ("<left-fringe> <mouse-4>" . bm-previous-mouse)
     ("<left-margin> <mouse-4>" . bm-previous-mouse)
     ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
     ("<left-margin> <mouse-1>" . bm-toggle-mouse))))

(use-package bookmark+
  :commands (bmkp-desktop-jump))

;; Quickly save and restore point using registers
(defun modi/save-point (restore)
  "Save the current point to a bookmark.

If RESTORE is non-nil, restore the saved point."
  (interactive "P")
  (let ((reg-char ?🖝)
        (text (if restore
                  (list "Restored" "from")
                (list "Saved" "to")))
        (message-log-max nil))
    (if restore
	
        (jump-to-register reg-char)
      (point-to-register reg-char))
    (message "%s point %s register %s (%s)."
             (nth 0 text)
             (nth 1 text)
             (get-char-code-property reg-char 'name)
             (char-to-string reg-char))))

(defun modi/restore-point ()
  "Restore saved point from register."
  (interactive)
  (modi/save-point :restore))

(bind-keys
 :map modi-mode-map
  ("<M-home>" . modi/save-point)
  ("<M-end>"  . modi/restore-point))


(provide 'setup-bookmarks)
;;; setup-bookmarks ends here
