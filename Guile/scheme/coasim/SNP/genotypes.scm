
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>


;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim SNP genotypes)">
;;  <brief>
;;    This module contains functions for manipulating SNP (unphased) genotypes.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:

(define-module (coasim SNP genotypes) 
  :use-module (ice-9 optargs)
  :use-module ((coasim markers) :select ((split-in-cases-controls . split))))


(define-public (haplotypes->genotypes lst)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='haplotypes->genotypes'>
    <brief>Translate a list of SNP haplotypes into a list of SNP genotypes.</brief>
    <prototype> (haplotypes->genotypes haplotype-list)</prototype>
    <example> (use-modules (coasim rand) (coasim SNP genotypes))
 (define haplotypes (simulate-sequences (make-random-snp-markers 4 0 1) 8))
 (display haplotypes)(newline)
 (define genotypes (haplotypes->genotypes haplotypes))
 (display genotypes)(newline) </example>
    <description>
     <p>
      Translate a list containing an even number of SNP haplotypes
      into a list of SNP genotypes.  The haplotypes are paired and the
      alleles are translated such that homozygote 00 becomes 0, homozygote
      11 becomes 1, and heterozygotes become 2.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (let* ((combine-alleles
	  (lambda (a1 a2) 
	    (cond ((and (= a1 0) (= a2 0)) 0)
		  ((and (= a1 1) (= a2 1)) 1)
		  (else 2))))
	 (combine-haplotypes
	  (lambda (h1 h2)
	    (map combine-alleles h1 h2))))
	   
    (letrec ((first 
	      (lambda (lst acc) 
		(cond ((null? lst) acc)
		      (else (second (car lst) (cdr lst) acc)))))
	     (second
	      (lambda (f lst acc)
		(first (cdr lst)
		       (cons (combine-haplotypes f (car lst)) acc)))))
      (reverse (first lst '())))))


(define-public (split-in-cases-controls genotypes trait-idx . args)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='split-in-cases-controls'>
    <brief>Split a list of genotypes into cases and controls.</brief>
    <prototype>(split-in-cases-controls genotypes trait-idx)</prototype>
    <example> (define haplotypes (simulate-sequences markers 100))
 (define genotypes (haplotypes->genotypes haplotypes))
 (define cases-and-controls (split-in-cases-controls genotypes trait-idx
                                                     :disease-model 'recessive))
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
       <li><b>homozygote-0-prob:</b> The probability that genotype 0 (or 00)
           is considered a case.  By default this is 0.
       </li>
       <li><b>homozygote-1-prob:</b> The probability that genotype 1 (or 11)
           is considered a case.  By default this is 1.
       </li>
       <li><b>heterozygote-prob:</b> The probability that genotype 2 (01 or 10)
           is considered a case.  By default this is 0.5.
       </li>
       <li><b>disase-model:</b> Should be either 'recessive or 'dominant;
           if 'dominant genotypes 1 (11) and 2 (01 or 10) are considered cases,
           if 'recessive only genotypes 1 (11) are considered cases.
           only 
       </li>
       <li><b>remove-trait:</b> If #t the alleles at the trait marker are
         removed, if #f they are kept.
       </li>
     </ul>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (let-keywords args #f ((disease-model 'unspecified)
			 (homozygote-0-prob 0)
			 (homozygote-1-prob 1)
			 (heterozygote-prob 0.5)
			 (remove-trait #t))
      (let* ((msec (cdr (gettimeofday)))
	     (random-state (seed->random-state msec))
	     (is-case? 
	      (lambda (a)
		(let ((r (random 1.0 random-state)))
		  (cond ((= a 0) (< r homozygote-0-prob))
			((= a 1) (< r homozygote-1-prob))
			((= a 2) (< r heterozygote-prob))
			(else (throw 'unknown-allele a)))))))

	(if (not (member disease-model '(unspecified recessive dominant)))
	    (throw 'unknown-disease-model disease-model))

	(if (eq? disease-model 'recessive)
	    (begin (set! homozygote-1-prob 1)
		   (set! heterozygote-prob 0)))
	(if (eq? disease-model 'dominant)
	    (begin (set! homozygote-1-prob 1)
		   (set! heterozygote-prob 1)))

	(split genotypes trait-idx is-case? :remove-trait remove-trait))))


;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
