
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of a SNP haplotype simulation, where the resulting
;;; haplotypes are separated into a set of affected (disease cases)
;;; and a set of unaffected (controls) based on a trait marker.


;; Load functionality from modules
(use-modules ((srfi srfi-1)
	      :select (list-index))
             ((coasim rand)
	      :select (make-random-snp-markers
		       make-random-trait-markers))
	     ((coasim markers)        
	      :select (merge-markers))
	     ((coasim io)
	      :select (sequences-printer marker-positions-printer))
	     ((coasim disease-modelling) 
	      :select (split-in-cases-controls-on-markers)))


(define rho 400) ; rho=400 ... for pop.size ~10,000 this is a recombination
		 ; rate of 0.01, or about a centi-Morgan

;; Make SNP markers the random positions, accepting the SNP mutations
;; if between 10% and 90% of the final chromosomes have the mutant.
(define snp-markers (make-random-snp-markers 10 0.1 0.9))

;; Make two trait markers, each with mutant allele in 20% to 50%
;; chromosomes of the sample
(define disease-markers  (make-random-trait-markers 2 0.20 0.50))

;; Total list of markers is the combined SNP and trait markers
(define markers (merge-markers snp-markers disease-markers))

;; get indices of the two trait markers
(define trait-indices 
  (let ((trait-1-idx (list-index (lambda (m) (eq? m (car  disease-markers))) markers))
	(trait-2-idx (list-index (lambda (m) (eq? m (cadr disease-markers))) markers)))
    (list trait-1-idx trait-2-idx)))


;; Split in cases and controls, based on trait markers, using
;; probabilities:
;;
;; P(affected | both wildtype) = 0.1
;; P(affected | one mutant)    = 0.3
;; P(affected | both mutants)  = 0.5
(define split
  (lambda (haplotypes)
    (let* ((risk-0 0.1)
	   (risk-1 0.3)
	   (risk-2 0.5)
	   (risk (lambda (a1 a2)
		   (let ((no-mutants (+ a1 a2)))
		     (case no-mutants
		       ((0) risk-0)
		       ((1) risk-1)
		       ((2) risk-2)))))
	   
	   (msec (cdr (gettimeofday)))
	   (random-state (seed->random-state msec))
	   (is-case? (lambda (a1 a2)
		       (< (random 1.0 random-state) (risk a1 a2)))))
      (split-in-cases-controls-on-markers haplotypes trait-indices is-case?))))


;; Simulate to get 100 haplotypes
(define haplotypes (simulate-sequences markers 100 :rho rho))
;; then split in cases/controls
(define cases-and-controls (split haplotypes))
(define cases    (car  cases-and-controls))
(define controls (cadr cases-and-controls))

(call-with-output-file "snp-positions.txt" 
  (marker-positions-printer snp-markers))
(call-with-output-file "trait-positions.txt" 
  (marker-positions-printer disease-markers))
(call-with-output-file "cases.txt"    (sequences-printer cases))
(call-with-output-file "controls.txt" (sequences-printer controls))
