;;; init-c.el --- C stuff -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(require 'init-prog)

;; c-derived modes use `c-tab-always-indent' (why god?)
;; which doesn't support `complete' as `tab-always-indent' does.
;; https://www.reddit.com/r/emacs/comments/of6u95/ccmode_completion_at_point_using_tab_key_not/
;; Let's hack it to try to complete if indent fails.
(defadvice c-indent-line-or-region
    (around complete-at-point-if-indent-fails activate)
  "If `\(point\)' is the same after function, call `completion-at-point'."
  (let ((point-before (point)))
    ad-do-it
    (when (= point-before (point))
      (completion-at-point))))

(provide 'init-c)
;;; init-c.el ends here
