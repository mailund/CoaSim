
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim batch)">
;;  <brief>
;;    This module contains functions for running batchs of simulations.
;;    Although most of the functionallity in this module can be easily 
;;    implemented using Scheme's control-flow structures, the functions here
;;    make batch running just a tad simpler.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:

(define-module (coasim batch) :use-syntax (ice-9 syncase))


;; --<GUILE COMMENT>---------------------------------------------
;; <method name='repeat'>
;;  <brief>Call a block of code a certain number of times.</brief>
;;  <prototype>(repeat n c)</prototype>
;;  <example> (use-modules (coasim batch))
;; (repeat 20
;;  (let* ((m (snp-marker 0.5 0 1))
;;         (arg (simulate (list m) 10 :random-seed 10))
;;         (tree (car (local-trees arg))))
;;    (display tree)))
;; (newline)
;; 
;; (define (f)
;;  (let* ((m (snp-marker 0.5 0 1))
;;         (arg (simulate (list m) 10 :random-seed 10))
;;         (tree (car (local-trees arg))))
;;    (display tree)))
;; 
;; (repeat 20 f)
;; (newline) </example>
;;  <description>
;;   <p>
;;    Execute the code `c' `n' number of times.  `c' can either be a function,
;;    as `f' in the example above, or it can be a block of code as the `let*'
;;    expression above.
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 

(define-syntax repeat
  (syntax-rules ()
    ((_ n c)
     (let ((f (if (procedure? c) c (lambda () c))))
       (let loop ((m n))
	 (if (= m 0) #f
	     (begin (f) (loop (- m 1)))))))))
(export repeat)


;; --<GUILE COMMENT>---------------------------------------------
;; <method name='repeat-while'>
;;  <brief>Call a block of code until a predicate evaluates to false.</brief>
;;  <prototype>(repeat-while [p] c)</prototype>
;;  <example> (use-modules (ice-9 format) (coasim batch))
;;
;; (repeat-while (lambda (branch-length) (&lt; branch-length 5))
;;  (let* ((m (snp-marker 0.5 0 1))
;;         (arg (simulate (list m) 10))
;;         (tree (car (local-trees arg)))
;;         (branch-length (total-branch-length tree)))
;;    (format #t "~4f " branch-length)
;;    branch-length))
;; (newline)
;; 
;; (define (f)
;;   (let* ((m (snp-marker 0.5 0 1))
;;          (arg (simulate (list m) 10))
;;          (tree (car (local-trees arg)))
;;          (branch-length (total-branch-length tree)))
;;     (format #t "~4f " branch-length)
;;     branch-length))
;; (define (p branch-length)
;;   (&lt; branch-length 5))
;; 
;; (repeat-while p f)
;; (newline)
;; 
;; (repeat-while
;;  (let* ((m (snp-marker 0.5 0 1))
;;         (arg (simulate (list m) 10))
;;         (tree (car (local-trees arg)))
;;         (branch-length (total-branch-length tree)))
;;    (format #t "~4f " branch-length)
;;    (&lt; branch-length 5)))
;; (newline)</example>
;;  <description>
;;   <p> Execute the code `c' until the predicate `p', evaluated on
;;    the result of `c' evaluates to false. `c' can either be a
;;    function, as `f' in the example above, or it can be a block of
;;    code as the `let*' expression above.  If `c' itself evaluates to
;;    a boolean, the predicate can be left out and the execution will
;;    continue as long as `c' evalutes to true.
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax repeat-while
  (syntax-rules ()
    ((_ p c)
     (let ((f (if (procedure? c) c (lambda () c))))
       (let loop ()
	 (if (p (f)) 
	     #f
	     (loop)))))
    ((_ c)
     (let ((f (if (procedure? c) c (lambda () c))))
       (let loop ()
	 (if (f) 
	     #f
	     (loop)))))))
(export repeat-while)

;; --<GUILE COMMENT>---------------------------------------------
;; <method name='tabulate'>
;;  <brief>Call a block of code a certain number of times, collecting
;;         the results in a list.</brief>
;;  <prototype> (tabulate n c)
;; (tabulate n t c)
;; (tabulate n t p c)</prototype>
;;  <example> (use-modules (coasim batch))
;; (let* ((no-iterations 10000)
;;        (branch-lengths
;;         (tabulate no-iterations
;;          (let* ((m (snp-marker 0.5 0 1))
;;                 (arg (simulate (list m) 10))
;;                 (tree (car (local-trees arg)))
;;                 (branch-length (total-branch-length tree)))
;;            branch-length))))
;;   (display (/ (apply + branch-lengths) no-iterations))(newline))
;; 
;; (define (simulate-branch-length)
;;   (let* ((m (snp-marker 0.5 0 1))
;;          (arg (simulate (list m) 10))
;;          (tree (car (local-trees arg)))
;;          (branch-length (total-branch-length tree)))
;;     branch-length))
;; 
;; (let* ((no-iterations 10000)
;;        (branch-lengths (tabulate no-iterations simulate-branch-length)))
;;   (display (/ (apply + branch-lengths) no-iterations))(newline))  
;;
;; ;; Using transformer to translate arg into a branch-length
;; (let* ((no-iterations 10000)
;;        (branch-lengths
;; 	   (tabulate no-iterations
;;                     ;; transformer: arg -&gt; branch-length
;; 	             (lambda (arg) (total-branch-length (car (local-trees arg))))
;; 	             (simulate (list (snp-marker 0.5 0 1)) 10))))
;;   (display (/ (apply + branch-lengths) no-iterations))(newline))
;;
;; ;; Using transformer to translate arg into a branch-length, and
;; ;; a predicate to only include args with a single recombination
;; (use-modules (ice-9 format))
;; (let* ((no-iterations 10000)
;;        (branch-lengths
;;         (tabulate no-iterations
;;                   ;; transformer: arg -&gt; branch-length
;;                   (lambda (arg) (total-branch-length (car (local-trees arg))))
;;                   ;; predicate, only accepting ARGs with a single
;;                   ;; recombination
;;                   (lambda (arg) (= 1 (no-recombinations arg)))
;;                   (simulate (list (snp-marker 0.5 0 1)) 10 :rho 0.3))))
;;   (format #t "~d out of ~d ARGs had a single recombination.\n"
;;        (length branch-lengths) no-iterations)
;;   (format #t "The average branch length was: ~4f.\n"
;;        (/ (apply + branch-lengths) (length branch-lengths)))) </example>
;;  <description>
;;   <p>Execute the code `c' `n' number of times, collecting the
;;    results of evaluating `c' in a list that is returned as the
;;    result of the tabulation.  `c' can either be a function, as
;;    `simulate-branch-lehgth' in the example above, or it can be a
;;    block of code as the `let*' expression above.
;;   </p>
;;   <p> As a short-cut, two special cases are supported: using a
;;    transformer-function to translate the simulation result into the
;;    value to be collected, and combining a transformer and a
;;    predicate, so only transformed values, for the simultaion
;;    results accepted by the predicate, are collected.  That is, the
;;    predicate is evaluated on the simulation result and the
;;    transformer is only applied if the predicate accepts the
;;    simulation result.
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax tabulate
  (syntax-rules ()
    ((_ n c)
     (let ((f (if (procedure? c) c (lambda () c))))
       (let loop ((m n)
		  (acc '()))
	 (if (= m 0) (reverse acc)
	     (let* ((x (f)) 
		    (new-acc (cons x acc)))
	       (loop (- m 1) new-acc))))))

    ((_ n t c)
     (let ((f (if (procedure? c) c (lambda () c))))
       (let loop ((m n)
		  (acc '()))
	 (if (= m 0) (reverse acc)
	     (let* ((x (t (f)))
		    (new-acc (cons x acc)))
	       (loop (- m 1) new-acc))))))

    ((_ n t p c)
     (let ((f (if (procedure? c) c (lambda () c))))
       (let loop ((m n)
		  (acc '()))
	 (if (= m 0) (reverse acc)
	     (let* ((x (f)))
	       (if (p x)
		   (loop (- m 1) (cons (t x) acc))
		   (loop (- m 1) acc)))))))))
(export tabulate)

;; --<GUILE COMMENT>---------------------------------------------
;; <method name='fold'>
;;  <brief>
;;   Call a block of code a certain number of times, collecting the result
;;   using a combination function.
;;  </brief>
;;  <prototype>(fold n comb init code)</prototype>
;;  <example> (let* ((no-iterations 10000)
;;        (branch-sum
;;         (fold no-iterations (lambda (val sum) (+ val sum)) 0
;;          (let* ((m (snp-marker 0.5 0 1))
;;                 (arg (simulate (list m) 10))
;;                 (tree (car (local-trees arg)))
;;                 (branch-length (total-branch-length tree)))
;;            branch-length))))
;;   (display (/ branch-sum no-iterations))(newline))</example>
;;  <description> <p> Execute the code `code' `n' number of times,
;;   combining the result of `code' using the `comb' function.  The
;;   `comb' is called with two arguments in each iteration, the first
;;   is the result of having run `code', the second is the result of
;;   the previous combination, initially the value `init' passed to
;;   `fold'.  </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax fold
  (syntax-rules ()
    ((_ n comb init code)
     (let ((f (if (procedure? code) code (lambda () code))))
       (let loop ((m n) (acc init))
	 (if (= m 0) acc
	     (let* ((x (f)) 
		    (new-acc (comb x acc)))
	       (loop (- m 1) new-acc))))))))
(export fold)
  


;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
