
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating several coalescence trees and calculating
;;; their mean branch-length

(use-modules ((coasim batch) :select (tabulate)))
(let* ((no-iterations 10000)
       (branch-lengths
	(tabulate no-iterations
		  (let* ((m (snp-marker 0.5 0 1))
			 (arg (simulate (list m) 10))
			 (tree (car (local-trees arg))))
		    (total-branch-length tree)))))
  (display (/ (apply + branch-lengths) no-iterations))(newline))
