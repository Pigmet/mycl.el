;; for clojure code editing.

(defun mycl-comment-below()
  (interactive)
  (save-excursion
    (insert "(comment")
    (end-of-buffer)
    (insert ")")))

