
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
    <example> (define haplotypes (simulate-sequences markers 100))
 (define cases-and-controls (split-in-cases-controls haplotypes trait-idx))
 (define cases    (car  cases-and-controls))
 (define controls (cadr cases-and-controls))</example>
    <description>
     <p>
      Split a dataset into cases and controls, based on the value at
      trait-idx.   
     </p>
     <p>
      By default the alleles at the trait marker is removed from the resulting
      lists; an optional parameter, :remove-trait, if set to #f, will prevent
      removal of the trait marker.
     </p>
     <p>
      The following keyword arguments can be used to control
      the split:
     </p>
     <ul>
       <li><b>mutant-prob:</b> The probability that a mutant (allele 1) is 
           considered a case.  By default this is 1.
       </li>
       <li><b>wild-type-prob:</b> The probability that a wild-type (allele 0) 
           is considered a case.  By default this is 0.
       </li>
       <li><b>remove-trait:</b> If #t the alleles at the trait marker are
         removed, if #f they are kept.
       </li>
     </ul>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (let-keywords args #f ((mutant-prob 1)
			 (wild-type-prob 0)
			 (remove-trait #t))
      (let* ((msec (cdr (gettimeofday)))
	     (random-state (seed->random-state msec))
	     (is-case? 
	      (lambda (a) 
		(if (= 1 a)
		    (< (random 1.0 random-state) mutant-prob)
		    (< (random 1.0 random-state) wild-type-prob)))))
	(split haplotypes trait-idx is-case? :remove-trait remove-trait))))

;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
