;; for clojure code editing.

(defun mycl-comment-below()
  (interactive)
  (save-excursion
    (insert "(comment")
    (end-of-buffer)
    (insert ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java import -> clojure import ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq import-regex
      (rx "import"
	  (+ space)
	  (group-n 1 (+ anything))
	  "."
	  (group-n 2 (+ word) )))

(defun mycl--parse-import-line (s)
  "Returns a list of (path class)."
  (mylet [((_ path class)) (s-match-strings-all import-regex s)]
	 (list path class)))

(defun mycl--import-sentences (s)
  (->> s
       (s-split ";")
       (-map 's-trim)
       (-filter (-lambda (s) (s-matches-p import-regex s)))
       (-map 'mycl--parse-import-line)
       (-map (-lambda ((path cl))
	       (format "(%s %s)" path cl)))
       (s-join "\n")))

(defun mycl-clojure-import ()
  "Converts Java import semtences to Clojure ones."
  (interactive)
  (if (use-region-p)
      
      (mylet [beg (region-beginning)
		  end (region-end)
		  res
		  (-> (buffer-substring-no-properties beg end)
		      mycl--import-sentences
		      kill-new)]
	     (message (format "Copied %s." res)))

    (error "no region is active")))

(provide 'mycl)
