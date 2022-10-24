;;; init-frames.el --- Frame configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst mabo3n/frame-default-size '(100 . 40)
  "Cons cell with default `(WIDTH . HEIGHT)' for frames.")

(defun mabo3n/frame-resize (&optional frame size)
  "Resize FRAME to SIZE and return FRAME.

FRAME is a frame object and SIZE has form (WIDTH . HEIGHT).

If not specified, FRAME defaults to `selected-frame' and
SIZE defaults to `mabo3n/frame-default-size'.

When called interactively, prompts for WIDTH and HEIGHT."
  (interactive (list nil
                     (let ((default mabo3n/frame-default-size))
                       (cons (read-number "Width: "  (car default))
                             (read-number "Height: " (cdr default))))))
  (let ((size  (or size mabo3n/frame-default-size))
        (frame (or frame (selected-frame))))
    (set-frame-size frame (car size) (cdr size))
    frame))

;; From `https://christiantietze.de/posts/2021/06/emacs-center-window-single-function/'
(defun mabo3n/recenter-frame (&optional frame)
  "Center FRAME on the screen.

FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                           frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           ;;             HACK Ignore extra displays in my work (WSL) setup
           (monitor-w (if mabo3n/wsl 1920
                        (nth 2 (frame-monitor-workarea frame))))
           (monitor-h (if mabo3n/wsl 1080
                        (nth 3 (frame-monitor-workarea frame))))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position `(,frame ,@center)))))

(defun mabo3n/reset-frame (&optional frame)
  "Resize and recenter FRAME.

See `mabo3n/frame-resize' and `mabo3n/recenter-frame'."
  (interactive)
  (mabo3n/frame-resize)
  (mabo3n/recenter-frame))

(mabo3n/reset-frame)
(add-hook 'after-make-frame-functions #'mabo3n/frame-resize)

(spacemacs/declare-prefix "F r" "Re-")

(spacemacs/set-leader-keys
  "F F" #'make-frame
  "F <tab>" #'other-frame
  "F m" #'spacemacs/toggle-maximize-frame
  "F r c" #'mabo3n/recenter-frame
  "F r s" #'mabo3n/frame-resize
  "F r r" #'mabo3n/reset-frame)

(provide 'init-frames)
;;; init-frames.el ends here
