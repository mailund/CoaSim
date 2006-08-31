
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim statistics)">
;;  <brief>
;;    This module contains functions for calculating various statistics
;;    on the ARG and on lists of haplotypes.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

(define-module (coasim statistics) 
  :use-module ((coasim) :select (recombination-node? 
				 gene-conversion-node?
				 coalescent-node?
				 fold-nodes))
  :use-module (srfi srfi-1))


(define (count-nodes arg p)
  (fold-nodes arg 
	      (lambda (n count) (if (p n) (+ count 1) count)) 0))

;;--<GUILE COMMENT>---------------------------------------------
;;<method name="no-recombinations">
;;  <brief>Returns the number of recombinations in the ARG.</brief>
;;  <prototype>(no-recombinations arg)</prototype>
;;  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
;;(define arg (simulate markers 100 :rho 400))
;;(define n (no-recombinations arg))</example>
;;  <description>
;;    <p>
;;     Returns the number of recombinations in the ARG.
;;    </p>
;;  </description>
;;</method>
;;
;;<method name="no-gene-conversions">
;;  <brief>Returns the number of gene conversions in the ARG.</brief>
;;  <prototype>(no-gene-conversions arg)</prototype>
;;  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
;;(define arg (simulate markers 100 :gamma 10 :Q 0.2))
;;(define n (no-gene-conversions arg))</example>
;;  <description>
;;    <p>
;;     Returns the number of gene conversions in the ARG.
;;    </p>
;;  </description>
;;</method>
;;
;;<method name="no-coalescence-events">
;;  <brief>Returns the number of coalescence events in the ARG.</brief>
;;  <prototype>(no-coalescence-events arg)</prototype>
;;  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
;;(define arg (simulate markers 100 :rho 400))
;;(define n (no-coalescence-events arg))</example>
;;  <description>
;;    <p>
;;     Returns the number of coalescence events in the ARG.
;;    </p>
;;  </description>
;;</method>
;;-----</GUILE COMMENT>--------------------------------------------
(define-public (no-coalescence-events arg)
  (count-nodes arg coalescent-node?))
(define-public (no-recombinations arg)   
  (/ (count-nodes arg recombination-node? ) 2))
(define-public (no-gene-conversions arg)
  (/ (count-nodes arg gene-conversion-node? ) 2))


(define (all-equal lst)
  (every (lambda (e) (= e (car lst))) (cdr lst)))

(define (transpose-list lst)
  (let* ((a (list->array 2 lst))
	 (t (transpose-array a 1 0)))
    (array->list t)))

(define (all-pairs list-of-lists)
  (letrec ((f (lambda (result 1st rest)
		(if (null? rest) result
		    (let ((1st+rest (map (lambda (r) (list 1st r)) rest)))
		      (f (append 1st+rest result) (car rest) (cdr rest)))))))
    (f '() (car list-of-lists) (cdr list-of-lists))))



;;--<GUILE COMMENT>---------------------------------------------
;;<method name="S">
;;  <brief>The number of mutations in a sample.</brief>
;;  <prototype>(S lst)</prototype>
;;  <example>(S '((0 1 1 1 1 0) (0 0 1 1 1 0) (0 1 1 0 1 0) (0 0 0 1 1 0)))</example>
;;  <description>
;;    <p>
;;     Calculates the number of mutations in a sample (calculated as the
;;     number of informative sites).
;;    </p>
;;  </description>
;;</method>
;;-----</GUILE COMMENT>--------------------------------------------
(define-public (S lst)
  (length (filter (lambda (e) (not e))
		  (map all-equal (transpose-list lst)))))


;;--<GUILE COMMENT>---------------------------------------------
;;<method name="pi_ij">
;;  <brief>Calculates the number of differences between two haplotypes.</brief>
;;  <prototype>(pi_ij si sj)</prototype>
;;  <example>(pi_ij '(0 1 1 0) '(1 0 1 0))</example>
;;  <description>
;;    <p>
;;     Calculates the number of differences between two haplotypes.
;;    </p>
;;  </description>
;;</method>
;;-----</GUILE COMMENT>--------------------------------------------
(define-public (pi_ij si sj)
  (length (filter (lambda (p) (not (= (car p) (cadr p)))) (zip si sj))))

;;--<GUILE COMMENT>---------------------------------------------
;;<method name="pi">
;;  <brief>Calculates the average pairwise difference.</brief>
;;  <prototype>(pi list-of-haplotypes)</prototype>
;;  <example>(pi '((0 1 1 0) (1 0 1 0) (1 0 1 1)))</example>
;;  <description>
;;    <p>
;;     Calculates the average pairwise difference on a list of haplotypes.
;;    </p>
;;  </description>
;;</method>
;;-----</GUILE COMMENT>--------------------------------------------
(define-public (pi lst)
  (let* ((n (length lst))
	 (get-dif (lambda (p) (pi_ij (car p) (cadr p))))
	 (difs (map get-dif (all-pairs test-seqs)))
	 (sum (apply + difs)))
    (/ (* 2 sum) (* n (- n 1)))))


(define (a n)
  (let loop ((res 0) (i 1))
    (if (= i n) res
	(loop (+ res (/ 1 i)) (+ i 1)))))

;;--<GUILE COMMENT>---------------------------------------------
;;<method name="theta_W">
;;  <brief>Watterson's mutation rate estimate.</brief>
;;  <prototype>(theta_W lst)</prototype>
;;  <example>(theta_W '((0 1 1 1 1 0) (0 0 1 1 1 0) (0 1 1 0 1 0) (0 0 0 1 1 0)))</example>
;;  <description>
;;    <p>
;;     Calculates Watterson's mutation rate estimate.
;;    </p>
;;  </description>
;;</method>
;;-----</GUILE COMMENT>--------------------------------------------
(define-public (theta_W lst)
  (/ (S lst) (a (length lst))))

(define (b n)
  (let ((sq (lambda (x) (* x x))))
    (let loop ((res 0) (i 1))
      (if (= i n) res
	  (loop (+ res (/ 1 (sq i))) (+ i 1))))))

;;--<GUILE COMMENT>---------------------------------------------
;;<method name="D">
;;  <brief>Tajima's D statistics.</brief>
;;  <prototype>(D lst)</prototype>
;;  <example>(D '((0 1 1 1 1 0) (0 0 1 1 1 0) (0 1 1 0 1 0) (0 0 0 1 1 0)))</example>
;;  <description>
;;    <p>
;;     Calculates Tajima's D statistics on a list of haplotypes.
;;    </p>
;;  </description>
;;</method>
;;-----</GUILE COMMENT>--------------------------------------------
(define-public (D lst)
  (let* ((n (length lst))
	 (a_n (a n))
	 (a_n_sq (* a_n a_n))
	 (b_n (b n))
	 (S_n (S lst))

	 (pi_hat (pi lst))
	 (theta_W (/ S_n a_n))

	 (e1 (- (/ (+ n 1) (* 3 a_n (- n 1))) (/ 1 a_n_sq)))
	 (e2 (* (/ 1 (+ a_n_sq b_n))
		(+ (/ (* 2 (+ (* n n) n 3)) (* 9 n (- n 1))) 
		   (- (/ (+ n 2) (* n a_n)))
		   (/ b_n a_n_sq)))))

    (/ (- pi_hat theta_W) (sqrt (+ (* e1 S_n) (* e2 S_n (- S_n 1)))))))



;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
