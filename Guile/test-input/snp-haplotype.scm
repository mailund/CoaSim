
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Example of a SNP haplotype simulation.


;; load module for making random markers
(use-modules ((coasim io)   :select (sequences-printer
				     marker-positions-printer))
	     ((coasim rand) :select (make-random-snp-markers)))

(define rho 400) ; rho=400 ... for pop.size ~10,000 this is a mutation
		 ; rate of 0.01, or about a centi-Morgan

;; Make 10 SNP markers at random positions, accepting the SNP
;; mutations if between 10% and 90% of the final chromosomes have the
;; mutant.
(define markers (make-random-snp-markers 10 0.1 0.9))

;; simulate to get the arg and 100 haplotypes
(define seqs (simulate-sequences markers 100 :rho rho))

;; save the haplotypes
(call-with-output-file "snp-positions.txt" (marker-positions-printer markers))
(call-with-output-file "sequences.txt"     (sequences-printer seqs))
