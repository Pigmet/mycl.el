;; for clojure code editing.

(require 'a)

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
  "Returns an a-list of (path class)."
  (mylet [((_ path class)) (s-match-strings-all import-regex s)]
	 (a-list  path  class)))

(defun mycl--import-sentences (s)
  (->> s
       (s-split ";")
       (-map 's-trim)
       (-filter (-lambda (s) (s-matches-p import-regex s)))
       (-map 'mycl--parse-import-line)
       (apply 'a-merge-with (lambda (x y) (mystr x " " y)))
       (a-reduce-kv (lambda (acc k v)
		      (mystr acc  (format "(%s %s)\n" k v)))
		    "")))

(defun mycl-clojure-import ()
  "Converts Java import semtences in the current region to Clojure ones."
  (interactive)
  (if (use-region-p)
      (mylet [beg (region-beginning)
		  end (region-end)
		  res
		  (-> (buffer-substring-no-properties beg end)
		      mycl--import-sentences
		      kill-new)]
	     (message (format "Copied %s in the clipboard." res)))
    (error "no region is active")))

;;;;;;;;;;
;; boot ;;
;;;;;;;;;;

;; gettin gcompile error on 'boot cljs'. 

(defun mycl--make-dir (path)
  (shell-command (format "mkdir -p %s" path))
  path) 

;; TODO: simple command to do the same ->
;; mkdir -p modern-cljs/{src/cljs/modern_cljs,html}

(defun mycl--boot-make-dir
    (project-name)
  "Takes project name for a boot project and creates scaffold
directory structure. Returns an alist of the resulting paths."
  (mylet [dir (mystr default-directory  project-name "/")
	      dir (mycl--make-dir dir)
	      html (mycl--make-dir  (mystr dir "html/"))
	      src (mycl--make-dir  (mystr dir "src/cljs/" project-name "/"))]
	 (a-list :dir dir :html html :src src)))

(defun mycl--write-file (path s)
  (with-temp-file path (insert s)))

(defun mycl--read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun mycl--boot-new-project-impl (project-name)
  (mylet [p project-name
	    res (mycl--boot-make-dir p)
	    dir (a-get res :dir)
	    html (a-get res :html)
	    src (a-get res :src)
	    html-s (mycl--read-file (mystr default-directory "/boot/html"))
	    build-s (mycl--read-file (mystr default-directory "/boot/build"))]
	 (mycl--write-file (mystr dir "build.boot") build-s)
	 (mycl--write-file (mystr html "index.html") html-s)
	 (mycl--write-file (mystr src "core.cljs") "")
	 (message (format "new boot project %s was created." p))))

(defun mycl-boot-new-project()
  (interactive)
  (mylet [project-name (read-string "Create a new boot project: ")]
	 (mycl--boot-new-project-impl project-name)))

;;;;;;;;;;;;;;
;; leinigen ;;
;;;;;;;;;;;;;;

(setq leinigen-opts (a-list "normal" "" "app" "app"))

(defun mycl-new-project-leinigen()
  (interactive)
  (mylet [p (read-string "Enter project name: ")
	    k (ido-completing-read "Select option: " (a-keys leinigen-opts))
	    opt (a-get leinigen-opts k)]
	 (when (y-or-n-p (format "Create new project %s ?" p))
	   (shell-command
	    (format "lein new %s %s" opt p)))))






(provide 'mycl)

