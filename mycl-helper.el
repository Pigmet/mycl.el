;; java import -> clojure import

(defun myel--belong? (elt list)
  (-some? (-lambda (x) (equal elt x)) list))

(setq sample-text ";; import org.jfree.chart.ChartFactory;
;; import org.jfree.chart.ChartPanel;
;; import org.jfree.chart.JFreeChart;
;; import org.jfree.chart.StandardChartTheme;
;; import org.jfree.chart.axis.DateAxis;
;; import org.jfree.chart.plot.XYPlot;
;; import org.jfree.chart.renderer.xy.XYItemRenderer;
;; import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
;; import org.jfree.chart.ui.ApplicationFrame;
")

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

(s-match-strings-all import-regex
		     "import org.jfree.chart.ui.ApplicationFrame;")

(setq res1
      (->> sample-text
	   (s-split ";")
	   (-map 's-trim)
	   (-remove (-lambda (s) (-> s length (equal 0) )))
	   (-map 'mycl--parse-import-line)))


(provide 'mycl)
