
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim SNP shared)">
;;  <brief>
;;    This module contains functions for manipulating SNP data, both
;;    haplotype and genotype.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; This module contains functions for manipulating SNP data, both
;;; haplotype and genotype.

;;; Code:

(define-module (coasim SNP shared) 
  :use-module ((srfi srfi-1) :select (take drop)))

(define-public (split-in-cases-controls haplotypes trait-idx is-case?)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='split-in-cases-controls'>
    <brief>Split a list of haplotypes into cases and controls.</brief>
    <prototype>(split-in-cases-controls haplotypes trait-idx is-case?)</prototype>
    <example>(use-modules (coasim SNP shared))
    (let ((is-case? (lambda (h) (= 1 (list-ref h trait-idx)))))
    (split-in-cases-controls haplotypes trait-idx is-case?)))</example>
    <description>
     <p>
      Split a dataset into cases and controls, based on the value at
      trait-idx.  If the value at that index satisfy the is-case? predicate,
      the haplotype is considered a case, otherwise a control.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (letrec ((strip-trait (lambda (h)
			  (let ((first (take h trait-idx))
				(rest  (drop h (+ 1 trait-idx))))
			    (append first rest))))
	   (f (lambda (lst cases controls)
		(cond ((null? lst) (list cases controls))
 		      ((is-case? (car lst)) 
		       (f (cdr lst) 
			  (cons (strip-trait (car lst)) cases)
			  controls))
		      (else
		       (f (cdr lst) 
			  cases
			  (cons (strip-trait (car lst)) controls)))))))
    (f haplotypes '() '())))

;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
