
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Example of a SNP haplotype simulation.


;; load module for making random markers
(use-modules ((coasim rand) :select (make-random-snp-markers)))

(define rho 200) ; rho=200 ... for pop.size ~10,000 this is a mutation
		 ; rate of 0.01, or about a centi-Morgan
(define Q 0)     ; zero geneconversion mean  tract length
(define G 0)     ; zero geneconversion rate
(define beta 0)  ; no exponential growth

(define p (arg-parameters rho Q G beta))

;; Make 10 SNP markers at random positions, accepting the SNP
;; mutations if between 10% and 90% of the final chromosomes have the
;; mutant.
(define markers (make-random-snp-markers 10 0.1 0.9))

;; simulate to get the arg and 100 haplotypes
(define arg (simulate p markers 100))

;; save the haplotypes
(save-sequences arg "snp-haplotypes.txt")
