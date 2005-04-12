
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating several coalescence trees and calculating
;;; their mean height

(use-modules ((coasim batch) :select (tabulate)))
(let* ((no-iterations 10000)
       (tree-heights
	(tabulate no-iterations
		  (let* ((arg (simulate '() 10
					:beta 10
					:keep-empty-intervals #t))
			 (tree (car (local-trees arg))))
		    (tree-height tree)))))
  (display (/ (apply + tree-heights) no-iterations))(newline))
