
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating several coalescence trees and calculating
;;; their mean branch-length

(use-modules ((coasim batch) :select (fold)))

(define betas '(0 10 20))
(define (mean-branch-length beta)
  (let* ((no-iterations 10)
	 (branch-sum
	  (fold no-iterations (lambda (val sum) (+ val sum)) 0
		(let* ((m (snp-marker 0.5 0 1))
		       (arg (simulate (list m) 10 :beta beta))
		       (tree (car (local-trees arg)))
		       (branch-length (total-branch-length tree)))
		  branch-length))))
    (/ branch-sum no-iterations)))

(display (map mean-branch-length betas))(newline)
