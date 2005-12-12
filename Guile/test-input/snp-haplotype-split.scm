
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of a SNP haplotype simulation, where the resulting
;;; haplotypes are separated into a set of affected (disease cases)
;;; and a set of unaffected (controls) based on a trait marker.


;; Load functionality from modules
(use-modules ((coasim rand)           :select (make-random-snp-markers
					       make-random-trait-markers))
	     ((coasim markers)        :select (insert-sorted-idx))
	     ((coasim io)             :select (sequences-printer
					       marker-positions-printer))
	     ((coasim SNP haplotypes) :select (split-in-cases-controls-on-marker)))


(define rho 400) ; rho=400 ... for pop.size ~10,000 this is a recombination
		 ; rate of 0.01, or about a centi-Morgan

;; Make SNP markers the random positions, accepting the SNP mutations
;; if between 10% and 90% of the final chromosomes have the mutant.
(define snp-markers (make-random-snp-markers 10 0.1 0.9))

;; Make a trait marker, where the between 18% and 22% chromosomes are
;; mutant
(define disease-marker (car (make-random-trait-markers 1 0.18 0.22)))

;; Insert the trait marker in the sorted list of markers
(define markers-and-index (insert-sorted-idx snp-markers disease-marker))
(define markers   (car  markers-and-index))
(define trait-idx (cadr markers-and-index))

;; Simulate to get 100 haplotypes
(define haplotypes (simulate-sequences markers 100 :rho rho))

;; Split in cases and controls, based on trait marker
(define cases-and-controls
  (split-in-cases-controls-on-marker haplotypes trait-idx
				     :mutant-prob 0.5
				     :wild-type-prob 0.1))
(define cases    (car  cases-and-controls))
(define controls (cadr cases-and-controls))

(call-with-output-file "snp-positions.txt" 
  (marker-positions-printer snp-markers))
(call-with-output-file "trait-position.txt" 
  (marker-positions-printer (list disease-marker)))
(call-with-output-file "cases.txt"    (sequences-printer cases))
(call-with-output-file "controls.txt" (sequences-printer controls))
