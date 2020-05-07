;; for clojure code editing.

(require 'a)

(defun mycl--display-in-buff(buff s)
  (with-current-buffer buff
    (erase-buffer)
    (insert s))
  (switch-to-buffer-other-window buff))

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

(defun mycl-new-project-leinigen()
  (interactive)
  (mylet [ leinigen-opts
	   (a-list :normal
		   (lambda (s) (format "lein new %s"s ))
		   :app
		   (lambda (s) (format "lein new app %s" s))
		   :figwheel-main
		   (lambda (s)
		     (format
		      "lein new figwheel-main %s -- --reagent" s)))
	   
	   p (read-string "Enter project name: ")
	   
	   type (->> (a-keys leinigen-opts)
		     (-map 'mystr)
		     (ido-completing-read "Select type: ")
		     (intern-soft))
	   
	   cmd (funcall (a-get leinigen-opts type) p)]
	 (when (y-or-n-p
		(format "Create a new project in %s ?" default-directory))
	   (shell-command cmd))))

(setq mycl-buff (generate-new-buffer "*mycl*"))

(setq mycl-repl-option-figwheel
      ":repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}")



(setq mycl-cljs-deps-data
      (a-list :dependencies
	      (a-list
	       :om (list "org.omcljs/om" "1.0.0-beta1")
	       :piggieback (list "cider/piggieback" "0.4.2")
	       :react (list "cljsjs/react" "16.6.0-0")
	       :react-dom (list "cljsjs/react-dom" "16.6.0-0")
	       :cljss (list "clj-commons/cljss" "1.6.4")
	       :sabnolo (list "sablono" "0.8.6"))
	      :repl-options mycl-repl-option-figwheel
	      ))

(defun mycl--figwheel-hint-string()
  (mylet [(&alist :dependencies dep :repl-options repl) mycl-cljs-deps-data]
	 (with-temp-buffer
	   (insert ":dependencies\n\n")
	   (loop for (_ . v) in dep
		 for (name  ver) = v
		 do
		 (insert (format "[%s \"%s\"]\n"  name ver )))
	   (insert "\n\n:repl-options\n\n" repl)
	   (buffer-string))))

(defun mycl-figwheel-dep-hint ()
  "Copy and paste this in project.clj when starting a figweel-main project."
  (interactive)
  (mycl--display-in-buff mycl-buff
			 (mycl--figwheel-hint-string )))

;; convert maven depndency

(setq demo-test
      "<dependency>
<groupId>org.apache.pdfbox</groupId>
<artifactId>pdfbox</artifactId>
<version>1.8.3</version>
</dependency>")

(provide 'mycl)

