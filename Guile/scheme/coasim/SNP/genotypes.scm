
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
  :use-module ((coasim disease-modelling) 
	       :select ((split-in-cases-controls-on-marker . split))))


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



(define (base-split preprocess is-hw? is-he? is-hm? postprocess)
  (lambda (genotypes trait-idx . args)
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
		  (cond ((is-hw? a) (< r homozygote-0-prob))
			((is-he? a) (< r heterozygote-prob))
			((is-hm? a) (< r homozygote-1-prob))
			(else (throw 'unknown-allele a)))))))

	(if (not (member disease-model '(unspecified recessive dominant)))
	    (throw 'unknown-disease-model disease-model))
	
	(if (eq? disease-model 'recessive)
	    (begin (set! homozygote-1-prob 1)
		   (set! heterozygote-prob 0)))
	(if (eq? disease-model 'dominant)
	    (begin (set! homozygote-1-prob 1)
		   (set! heterozygote-prob 1)))

	(let ((cases/controls
	       (split (preprocess genotypes) trait-idx is-case? 
		      :remove-trait remove-trait)))
	  (list (postprocess (car  cases/controls))
		(postprocess (cadr cases/controls))))))))


;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='split-in-cases-controls-on-marker'>
;;    <brief>Split a list of genotypes into cases and controls.</brief>
;;    <prototype>(split-in-cases-controls-on-marker genotypes trait-idx)</prototype>
;;    <example> (define haplotypes (simulate-sequences markers 100))
;; (define genotypes (haplotypes->genotypes haplotypes))
;; (define cases-and-controls (split-in-cases-controls-on-marker genotypes trait-idx
;;                                                     :disease-model 'recessive))
;; (define cases    (car  cases-and-controls))
;; (define controls (cadr cases-and-controls))</example>
;;    <description>

;;     <p> Split a dataset into cases and controls, based on the value
;;      at trait-idx.
;;     </p>
;;     <p>
;;      By default the alleles at the trait marker is removed from the
;;      resulting lists; an optional parameter, :remove-trait, if set
;;      to #f, will prevent removal of the trait marker.
;;     </p>
;;     <p>
;;      The following keyword arguments can be used to control
;;      the split:
;;     </p>
;;     <ul>
;;       <li><b>homozygote-0-prob:</b> The probability that genotype 0 (or 00)
;;           is considered a case.  By default this is 0.
;;       </li>
;;       <li><b>homozygote-1-prob:</b> The probability that genotype 1 (or 11)
;;           is considered a case.  By default this is 1.
;;       </li>
;;       <li><b>heterozygote-prob:</b> The probability that genotype 2 (01 or 10)
;;           is considered a case.  By default this is 0.5.
;;       </li>
;;       <li><b>disase-model:</b> Should be either 'recessive or 'dominant;
;;           if 'dominant genotypes 1 (11) and 2 (01 or 10) are considered cases,
;;           if 'recessive only genotypes 1 (11) are considered cases.
;;           only 
;;       </li>
;;       <li><b>remove-trait:</b> If #t the alleles at the trait marker are
;;         removed, if #f they are kept.
;;       </li>
;;     </ul>
;;    </description>
;;   </method>


;;   <method name='split-in-cases-controls-on-marker/phased'>
;;    <brief>Split a list of genotypes into cases and controls.</brief>
;;    <prototype>(split-in-cases-controls-on-marker/phased haplotypes trait-idx)</prototype>
;;    <example> (define haplotypes (simulate-sequences markers 100))
;; (define cases-and-controls (split-in-cases-controls-on-marker haplotypes trait-idx
;;                                                     :disease-model 'recessive))
;; (define cases    (car  cases-and-controls))
;; (define controls (cadr cases-and-controls))</example>
;;    <description>
;;     <p>
;;      Split a dataset into cases and controls, based on the value at
;;      trait-idx.  The dataset consist of a list of haplotypes, with
;;      an even number of haplotypes; the haplotypes will be
;;      considered pairwise and split into cases/controls based on
;;      their <em>genotype</em> at the trait marker, rather than the
;;      haplotype.  The resulting cases will be the pairs of
;;      haplotypes considered cases and the controls will be the
;;      remaining pairs.
;;     </p>
;;     <p>
;;      By default the alleles at the trait marker is removed from the
;;      resulting lists; an optional parameter, :remove-trait, if set
;;      to #f, will prevent removal of the trait marker.
;;     </p>
;;     <p>
;;      The following keyword arguments can be used to control
;;      the split:
;;     </p>
;;     <ul>
;;       <li><b>homozygote-0-prob:</b> The probability that genotype 00
;;           is considered a case.  By default this is 0.
;;       </li>
;;       <li><b>homozygote-1-prob:</b> The probability that genotype 11
;;           is considered a case.  By default this is 1.
;;       </li>
;;       <li><b>heterozygote-prob:</b> The probability that genotype 01 or 10
;;           is considered a case.  By default this is 0.5.
;;       </li>
;;       <li><b>disase-model:</b> Should be either 'recessive or 'dominant;
;;           if 'dominant genotypes 1/1, 0/1 and 1/0 are considered cases,
;;           if 'recessive only genotypes 1/1 are considered cases.
;;           only 
;;       </li>
;;       <li><b>remove-trait:</b> If #t the alleles at the trait marker are
;;         removed, if #f they are kept.
;;       </li>
;;     </ul>
;;    </description>
;;   </method>

;;   -----</GUILE COMMENT>----------------------------------------- 

(define-public split-in-cases-controls-on-marker 
  (let ((id     (lambda (x) x))
	(is-hw? (lambda (a) (= a 0)))
	(is-he? (lambda (a) (= a 2)))
	(is-hm? (lambda (a) (= a 1))))
    (base-split id is-hw? is-he? is-hm? id)))

(define (pair-haplotypes haplotypes)
  ;; sanity check
  (if (odd? (length haplotypes)) (throw 'uneven-number-of-haplotypes))
  ;; pairing of haplotypes
  (letrec ((pair-haplotypes
	    (lambda (h1 h2) (map list h1 h2)))
	   (first
	    (lambda (lst acc)
	      (cond ((null? lst) acc)
		    (else (second (car lst) (cdr lst) acc)))))
	   (second
	    (lambda (f lst acc)
	      (first (cdr lst)
		     (cons (pair-haplotypes f (car lst)) acc)))))
    (reverse (first haplotypes '()))))

(define (unpair-genotypes genotypes)
  (letrec ((unpack-pairs-helper
	    (lambda (plist 1st 2nd)
	      (if (null? plist)
		  (list (reverse 1st) (reverse 2nd))
		  (let ((p (car plist)))
		    (unpack-pairs-helper (cdr plist) 
					 (cons (car p) 1st)
					 (cons (cadr p) 2nd))))))
	   (unpack-pairs
	    (lambda (plist) (unpack-pairs-helper plist '() '())))

	   (unpack-genotypes-helper
	    (lambda (genotypes acc)
	      (if (null? genotypes) 
		  (reverse acc)
		  (let ((p (unpack-pairs (car genotypes))))
		    (unpack-genotypes-helper (cdr genotypes)
					     (cons (cadr p)
						   (cons (car p) acc))))))))
    (unpack-genotypes-helper genotypes '())))		

(define-public split-in-cases-controls-on-marker/phased
  (let ((is-hw? (lambda (a) (equal? a '(0 0))))
	(is-he? (lambda (a) (list? (member a '((0 1) (1 0))))))
	(is-hm? (lambda (a) (equal? a '(1 1)))))
    (base-split pair-haplotypes is-hw? is-he? is-hm? unpair-genotypes)))



;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
