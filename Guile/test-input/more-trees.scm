
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating several coalescence trees and printing them
;;; to stdout.

(use-modules ((coasim batch) :select (repeat)))
(repeat 10 (let* ((arg (simulate '() 10 :keep-empty-intervals #t))
		  (tree (car (local-trees arg))))
	     (display tree)))
