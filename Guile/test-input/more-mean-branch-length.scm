
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating several coalescence trees and calculating
;;; their mean branch-length

(use-modules ((coasim batch) :select (tabulate)))

(define betas '(0 10 20))
(define (mean-branch-length beta)
  (let* ((no-iterations 1000)
	 (branch-lengths
	  (tabulate no-iterations
	    (let ((arg (simulate (list (snp-marker 0.5 0 1)) 10 :beta beta)))
	      (total-branch-length (car (local-trees arg)))))))
    (/ (apply + branch-lengths) no-iterations)))

(display (map mean-branch-length betas))(newline)
