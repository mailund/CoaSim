;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim disease-modelling)">
;;  <brief>
;;    This module contains functions for disease models, specifically for
;;    splitting simulated sequences in diseased and healthy individuals based
;;    on their haplotypes or genotypes.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

(define-module (coasim disease-modelling) 
  :use-module ((srfi srfi-1)   :select (zip))
  :use-module ((ice-9 receive) :select (receive))
  :use-module ((ice-9 optargs) :select (let-keywords let-optional)))


;; re-implemetation of partition from srfi-1 -- the existing one is
;; not tail-recursive leading to a stack overflow when large
;; haplotypes are processed.
(define (partition pred list)
  (let loop ((list list) (first '()) (second '()))
    (if (null? list) 
	(values (reverse first) (reverse second))
	(if (pred (car list))
	    (loop (cdr list) (cons (car list) first) second)
	    (loop (cdr list) first (cons (car list) second))))))


(define (process-indices sequence indices combine)
  "Calles combine on all elements in sequence, with a flag set to true
 if the element is on one of the indices."
  (let loop ((idx 0)
	     (rest-seq sequence)
	     (rest-idx indices)
	     (collected '()))
    (cond ((null? rest-seq)
	   (append (reverse collected) rest-seq))

	  ((null? rest-idx)
	   ;; combine the rest
	   (let loop ((rest rest-seq) (collected collected))
	     (if (null? rest)
		 (reverse collected)
		 (loop (cdr rest) (combine #f (car rest) collected)))))

	  (else (if (= idx (car rest-idx))
		    (loop (+ 1 idx) (cdr rest-seq) 
			  (cdr rest-idx) (combine #t (car rest-seq) collected))
		    (loop (+ 1 idx) (cdr rest-seq) 
			  rest-idx (combine #f (car rest-seq) collected)))))))

(define (strip-indices sequence indices)
  (process-indices sequence indices
		   (lambda (f s c) (if (not f) (cons s c) c))))

(define (take-indices sequence indices)
  (process-indices sequence indices
		   (lambda (f s c) (if f (cons s c) c))))



;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='project-function'>
;;    <brief>
;;     Projects a list of allels to a subset for a function call.
;;    </brief>
;;    <prototype>(project-function f indices)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (define is-case?
;;   (project-function (lambda (a1 a3) (= a1 1) (= a3 0)) '(1 3)))</example>
;;    <description>
;;     <p>
;;      Translates a function that takes a subset of allels as input, into a
;;      function that takes the full list of alleles.
;;     </p>
;;     <p>
;;      If function f takes, say, two alleles as input, and we want to call
;;      it on index 1 and 3, we can use function (project-function f '(1 3))
;;      that will work on the full allele list by calling f with allele 1 
;;      and 3.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (project-function f indices)
  (lambda (s) (apply f (take-indices s indices))))



;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='table->function'>
;;    <brief>
;;     Translates a table into a function usable by e.g. the qtl-on-markers
;;     function.
;;    </brief>
;;    <prototype>(table->function table)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (define f-hash
;;   (let ((h (make-hash-table 4)))
;;     (hash-create-handle! h '(0 0)  0)
;;     (hash-create-handle! h '(0 1)  0)
;;     (hash-create-handle! h '(1 0) .1)
;;     (hash-create-handle! h '(1 1) .5)
;;     (table->function h)))
;; 
;; (define f-alist
;;   (let ((t
;; 	 (acons '(0 0)  0
;; 	 (acons '(0 1)  0
;; 	 (acons '(1 0) .1
;; 	 (acons '(1 1) .5
;; 	 '()))))))
;;     (table->function t)))
;; 
;; (define f-alist
;;   (let ((t
;; 	 (acons '(1 0) .1
;; 	 (acons '(1 1) .5
;; 	 '()))))))
;;     (table->function t)))
;; 
;; (define f-alist
;;   (let ((t
;; 	 (acons '(1 0) .1
;; 	 (acons '(1 1) .5
;; 	 '()))))))
;;     (table->function t 0.1)))</example>
;;    <description>
;;     <p>
;;      Translates a table, either a hash table or an associative list, into
;;      a function that can be used with qtl-on-markers or 
;;      split-in-cases-controls-on-markers.
;;     </p>
;;     <p>
;;      An optional second parameter can be used to set the default value for
;;      the function to return when the table does not contain whatever
;;      value the function is called with.  By default this value is 0.0.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (table->function table . rest)
  (let-optional rest ((default 0.0))
    (let ((get (if (list? table) assoc-ref hash-ref)))
      (lambda s 
	(let ((val (get table s)))
	  (if val val default))))))



;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='remove-alleles'>
;;    <brief>
;;     Removes the alleles of the markers at `indices' from the sequences.
;;    </brief>
;;    <prototype>(remove-alleles sequences indices)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (remove-allele seqs trait-indices)</example>
;;    <description>
;;     <p>
;;      Removes the alleles at `inidices' from the sequences; 
;;      useful for example for removing the trait markers from a dataset.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (remove-alleles seqs indices)
  (map (lambda (s) (strip-indices s indices)) seqs))



;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='remove-allele'>
;;    <brief>Removes the alleles of the marker at `idx' from the sequences.</brief>
;;    <prototype>(remove-allele sequences idx)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (remove-allele seqs trait-idx)))</example>
;;    <description>
;;     <p>
;;      Removes the alleles at `idx' from the sequences; useful for example for
;;      removing a trait marker from a dataset.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (remove-allele seqs idx)
  (remove-alleles seqs (list idx)))



;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='split-in-cases-controls'>
;;    <brief>Split a list of sequences into cases and controls.</brief>
;;    <prototype> (split-in-cases-controls sequences is-case?)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (let* ((is-mutant? (lambda (a) (= 1 a)))
;;        (allele     (lambda (h idx) (list-ref h idx)))
;;        (is-case?   (lambda (h) (and (is-mutant? (allele h 2))
;;                                     (is-mutant? (allele h 4))))))
;;     (split-in-cases-controls seqs is-case?))</example>
;;    <description>
;;     <p>
;;      Split a dataset into cases and controls, based on the
;;      predicate is-case?. If a sequence satisfy the is-case? predicate, 
;;      the sequence is considered a case, otherwise a control.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (split-in-cases-controls sequences is-case?)
  (receive (cases controls) (partition is-case? sequences)
	   (list cases controls)))


;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='split-in-cases-controls-on-markers'>
;;    <brief>Split a list of sequences into cases and controls.</brief>
;;    <prototype> (split-in-cases-controls-on-markers sequences marker-indices is-case?)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (let ((is-case? (lambda (a2 a4) (and (= 1 a2) (= 1 a4)))))
;;     (split-in-cases-controls-on-markers seqs '(2 4) is-case?)))</example>
;;    <description>
;;     <p>
;;      Split a dataset into cases and controls, based on the values of the 
;;      alleles at indices marker-indices.  If thes value at those indices
;;      satisfy the is-case? predicate, the sequence is considered a case, 
;;      otherwise a control.
;;     </p>
;;     <p> By default the alleles at the marker indices are removed
;;      from the resulting lists; an optional parameter,
;;      <b>:remove-traits</b>, if set to #f, will prevent removal of
;;      the marker alleles.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (split-in-cases-controls-on-markers
		sequences marker-indices is-case? . kwargs)
  (let-keywords kwargs #f ((remove-traits #t))
    (let ((p (project-function is-case? marker-indices)))
      (receive (cases controls) (partition p sequences)
	       (if remove-traits 
		   (list (remove-alleles cases    marker-indices)
			 (remove-alleles controls marker-indices))
		   (list cases controls))))))


;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='split-in-cases-controls-on-marker'>
;;    <brief>Split a list of sequences into cases and controls.</brief>
;;    <prototype> (split-in-cases-controls-on-marker sequences marker-idx is-case?)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (let ((is-case? (lambda (a) (= 1 a))))
;;     (split-in-cases-controls-on-marker seqs marker-idx is-case?)))</example>
;;    <description>
;;     <p>
;;      Split a dataset into cases and controls, based on the value of the 
;;      marker at index marker-idx.  If the value at that index satisfy the
;;      is-case? predicate, the sequence is considered a case, otherwise a
;;      control.
;;     </p>
;;     <p> By default the alleles at the marker is removed from the
;;      resulting lists; an optional parameter, <b>:remove-trait</b>,
;;      if set to #f, will prevent removal of the marker alleles.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (split-in-cases-controls-on-marker 
		sequences marker-idx is-case? . kwargs)
  (let-keywords kwargs #f ((remove-trait #t))
     (split-in-cases-controls-on-markers sequences (list marker-idx) is-case?
					 :remove-traits remove-trait)))






;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='qtl-on-markers'>
;;    <brief>
;;      Calculates quantitative values for haplotypes based on selected 
;;      alleles.
;;    </brief>
;;    <prototype> (qtl-on-markers sequences marker-indices f)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (let ((qtl-value (lambda (a2 a4) (+ (* 2 a2) a4)))))
;;     (qtl-on-markers seqs '(2 4) qtl-value)))</example>
;;    <description>
;;     <p>
;;       Calculate a quantitative value for each sequence in sequences
;;       by calling f with the alleles at marker-indices for each sequence.
;;     </p>
;;     <p>
;;       Returns a list of lists, where the car is the sequence and the 
;;       cadr is the calculated value.
;;     </p>
;;     <p> By default the alleles at the marker indices are removed
;;      from the resulting lists; an optional parameter,
;;      <b>:remove-traits</b>, if set to #f, will prevent removal of
;;      the marker alleles.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (qtl-on-markers sequences marker-indices f . kwargs)
  (let-keywords kwargs #f ((remove-traits #t))
    (let* ((qtl (project-function f marker-indices))
	  (qtls (map qtl sequences)))
      (if remove-traits
	  (zip (remove-alleles sequences marker-indices) qtls)
	  (zip sequences qtls)))))


;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='split-on-threshold'>
;;    <brief>
;;      Split a list of sequences with associated quantitative values based on
;;      a threshold for the value.
;;    </brief>
;;    <prototype> (split-on-threshold sequence-qtl-list threshold)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (let* ((qtl-value (lambda (a2 a4) (+ (* 2 a2) a4)))
;;        (qtl-seqs (qtl-on-markers seqs '(2 4) qtl-value))
;;        (cases-controls (split-on-threshold qtl-seqs 1.5))
;;        (cases (car cases-controls))
;;        (controls (cadr cases-controls)))
;;    ...)</example>
;;    <description>
;;     <p>
;;      Split a list of sequences with associated quantitative values based on
;;      a threshold for the value.
;;     </p>
;;     <p>
;;       The input is a list of lists, where the first element in each list is
;;       a sequence and the second is a quantitative value.  This is the kind
;;       of values that qtl-on-markers return.
;;     </p>
;;     <p>
;;       The result is a list where the car contains the sequences where the
;;       associated value was below the threshold and where the cadr contains
;;       the sequences where the associated value was equal to or above the
;;       threshold.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (split-on-threshold seqs-and-qtl threshold)
  (let ((p (lambda (seq-qtl) (< (cadr seq-qtl) threshold))))
    (receive (below above) (partition p seqs-and-qtl)
	     (list (map car below) (map car above)))))

;;   --<GUILE COMMENT>---------------------------------------------
;;   <method name='split-on-probability'>
;;    <brief>
;;      Split a list of sequences with associated probabilities based on
;;      the probability.
;;    </brief>
;;    <prototype> (split-on-probability sequence-probability-list)</prototype>
;;    <example> (use-modules (coasim disease-modelling))
;; (let* ((qtl-value (lambda (a2 a4) (+ (* .2 a2) (* .1 a4))))
;;        (qtl-seqs (qtl-on-markers seqs '(2 4) qtl-value))
;;        (cases-controls (split-on-probability qtl-seqs))
;;        (cases (car cases-controls))
;;        (controls (cadr cases-controls)))
;;    ...)</example>
;;    <description>
;;     <p>
;;      Split a list of sequences with associated probabilities.
;;     </p>
;;     <p>
;;       The input is a list of lists, where the first element in each list is
;;       a sequence and the second is a probability.  The sequences are 
;;       separated into two output lists; for pair '(seq p), seq
;;       is put in the first list with probability p and in the second with
;;       probability 1-q.
;;     </p>
;;    </description>
;;   </method>
;;   -----</GUILE COMMENT>----------------------------------------- 
(define-public (split-on-probability seqs-and-p)
  (let* ((msec (cdr (gettimeofday)))
	 (random-state (seed->random-state msec))
	 (p (lambda (seq-p) (< (random 1.0 random-state) (cadr seq-p)))))
    (receive (below above) (partition p seqs-and-p)
	     (list (map car below) (map car above)))))




;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
