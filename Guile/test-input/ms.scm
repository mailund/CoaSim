
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Example of SNP haplotype simulations, with the result output to stdout
;;; in Hudson's ms format.


;; load module for making random markers
(use-modules ((coasim rand) :select (make-random-snp-markers))
	     ((coasim io)   :select (print-marker-positions)))


(define rho 400) ; rho=400 ... for pop.size ~10,000 this is a mutation
		 ; rate of 0.01, or about a centi-Morgan
(define Q 0)     ; zero geneconversion mean  tract length
(define G 0)     ; zero geneconversion rate
(define beta 0)  ; no exponential growth

(define p (arg-parameters rho Q G beta))

;; Simulate 3 datasets with 10 haplotypes with 5 markers
(define no-haplotypes 10)
(define no-markers     5)
(define no-datasets    3)

(define (print-list lst)
  "Prints a list `lst' of elements. "
  (let ((print-elm (lambda (elm) (display elm))))
    (for-each print-elm lst)
    (newline)))

(define-public (print-haplotypes haplotypes)
  "Print a list of haplotypes"
  (for-each print-list haplotypes))


(define (run-simulation)
  (let* (;; Make SNP markers at random positions, accepting the SNP
	 ;; mutations if between 10% and 90% of the final chromosomes have the
	 ;; mutant.
	 (markers (make-random-snp-markers no-markers 0.1 0.9))

	 ;; simulate to get the arg and haplotypes
	 (arg (simulate p markers no-haplotypes)))

    (newline)
    (display "//\n")
    (display "segsites: ")
    (display no-markers)(newline)
    (display "positions: ")
    (print-marker-positions (current-output-port) markers)
    (print-haplotypes (sequences arg))))

(do ((i 0 (+ i 1))) ((= i no-datasets) '())
  (run-simulation))
