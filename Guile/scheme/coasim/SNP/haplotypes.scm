
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>


;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim SNP haplotypes)">
;;  <brief>
;;    This module contains functions for manipulating SNP haplotypes.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:

(define-module (coasim SNP haplotypes) 
  :use-module (ice-9 optargs)
  :use-module ((coasim markers) :select ((split-in-cases-controls . split))))


(define-public (split-in-cases-controls haplotypes trait-idx . args)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='split-in-cases-controls'>
    <brief>Split a list of haplotypes into cases and controls.</brief>
    <prototype>(split-in-cases-controls haplotypes trait-idx)</prototype>
    <example> (define haplotypes (simulate-sequences p markers 100))
 (define cases-and-controls (split-in-cases-controls haplotypes trait-idx))
 (define cases    (car  cases-and-controls))
 (define controls (cadr cases-and-controls))</example>
    <description>
     <p>
      Split a dataset into cases and controls, based on the value at
      trait-idx. 
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (let-keywords args #f ((mutant-prob 1)
			 (wild-type-prob 0))
      (let* ((msec (cdr (gettimeofday)))
	     (random-state (seed->random-state msec))
	     (is-case? 
	      (lambda (h) 
		(if (= 1 (list-ref h trait-idx))
		    (< (random 1.0 random-state) mutant-prob)
		    (< (random 1.0 random-state) wild-type-prob)))))
	(split haplotypes trait-idx is-case?))))

;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
