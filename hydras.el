;;; hydras.el --- My hydras

;;; Commentary:
;;; Definition of hydras

;;; Code:
(require 'hydra)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
(global-set-key (kbd "<f2>") 'hydra-zoom/body)

(defhydra hydra-toggle (:color blue)
  "toggle"
  ("a" abbrev-mode "abbrev")
  ("d" toggle-debug-on-error "debug")
  ("f" auto-fill-mode "fill")a
  ("t" toggle-truncate-lines "truncate")
  ("w" whitespace-mode "whitespace")
  ("q" nil "cancel"))
(global-set-key (kbd "C-c C-v") 'hydra-toggle/body)


(defhydra hydra-splitter (global-map "C-c C-s")
  "splitter"
  ("h" hydra-move-splitter-left)
  ("j" hydra-move-splitter-down)
  ("k" hydra-move-splitter-up)
  ("l" hydra-move-splitter-right)
  )

(global-set-key (kbd "C-c C-s") 'hydra-splitter/body)
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left by ARG."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right by ARG."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up by ARG."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down by ARG."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(provide 'hydras)
;;; hydras.el ends here
