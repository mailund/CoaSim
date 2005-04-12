
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Example of an ARG simulation.

(use-modules ((ice-9 format) :select (format)))
(let ((ARG (simulate '() 10
		     :rho 40 :gamma 60 :Q 10
		     :keep-empty-intervals #t)))
  (format #t "#recomb: ~d, #gene conv.: ~d, #coalescent: ~d\n"
	  (no-recombinations     ARG)
	  (no-gene-conversions   ARG)
	  (no-coalescence-events ARG)))
