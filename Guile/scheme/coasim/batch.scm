
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
;; (newline) </example>
;;  <description>
;;   <p>
;;    Execute the code `c' `n' number of times.
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax repeat
  (syntax-rules ()
    ((_ n e1 e2 ...)
     (let loop ((m n))
       (if (= m 0) #f
	   (begin e1 e2 ... (loop (- m 1))))))))
(export repeat)



;; --<GUILE COMMENT>---------------------------------------------
;; <method name='repeat-while'>
;;  <brief>Call a block of code until a predicate evaluates to false.</brief>
;;  <prototype>(repeat-while p c)</prototype>
;;  <example> (use-modules (ice-9 format) (coasim batch))
;;
;; (repeat-while (lambda (branch-length) (&lt; branch-length 5))
;;  (let* ((m (snp-marker 0.5 0 1))
;;         (arg (simulate (list m) 10))
;;         (tree (car (local-trees arg)))
;;         (branch-length (total-branch-length tree)))
;;    (format #t "~4f " branch-length)
;;    branch-length))
;; (newline)</example>
;;  <description>
;;   <p> Execute the code `c' until the predicate `p', evaluated on
;;    the result of `c' evaluates to false. 
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax repeat-while
  (syntax-rules ()
    ((_ p e1 e2 ...)
     (let loop ()
       (if (p (begin e1 e2 ...)) (loop))))))
(export repeat-while)


;; --<GUILE COMMENT>---------------------------------------------
;; <method name='repeat-with-index'>
;;  <brief>Call a block of code a certain number of times.</brief>
;;  <prototype> (repeat-with-index (i n) c)
;; (repeat-with-index (i start stop) c)
;; (repeat-with-index (i start stop step) c) </prototype>
;;  <example> (use-modules (coasim batch))
;; (repeat-with-index (i 10)
;;    (let* ((markers (make-random-snp-markers 10 0 1))
;;           (seqs (simulate-sequences markers 10))
;;           (pos-file (string-append "positions." (number->string i) ".txt"))
;;           (seq-file (string-append "sequences." (number->string i) ".txt")))
;;      (call-with-output-file pos-file (marker-positions-printer markers))
;;      (call-with-output-file seq-file (sequences-printer seqs))))
;; 
;; (repeat-with-index (i 2 12)
;;    (let* ((markers (make-random-snp-markers 10 0 1))
;;           (seqs (simulate-sequences markers 10))
;;           (pos-file (string-append "positions." (number->string i) ".txt"))
;;           (seq-file (string-append "sequences." (number->string i) ".txt")))
;;      (call-with-output-file pos-file (marker-positions-printer markers))
;;      (call-with-output-file seq-file (sequences-printer seqs))))
;; 
;; (repeat-with-index (i 2 20 2)
;;    (let* ((markers (make-random-snp-markers 10 0 1))
;;           (seqs (simulate-sequences markers 10))
;;           (pos-file (string-append "positions." (number->string i) ".txt"))
;;           (seq-file (string-append "sequences." (number->string i) ".txt")))
;;      (call-with-output-file pos-file (marker-positions-printer markers))
;;      (call-with-output-file seq-file (sequences-printer seqs)))) </example>
;;  <description>
;;   <p> Execute the code `c' a number of times, making the index
;;    variable `i' visible to the code in `c'.  Comes in three
;;    variants, a simple iteration from 1 to `n', an iteration from
;;    `start' to `stop', and an iteration from `start' to `stop' in
;;    jumps of `step'.
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax repeat-with-index
  (syntax-rules ()
    ((_ (idx n) e1 e2 ...)
     (let ((f (lambda (idx) e1 e2 ...)))
       (let loop ((m 1))
         (if (> m n) #f
             (begin (f m) (loop (+ m 1)))))))

    ((_ (idx start stop) e1 e2 ...)
     (let ((f (lambda (idx) e1 e2 ...)))
       (let loop ((m start))
	 (if (> m stop) #f
	     (begin (f m) (loop (+ m 1)))))))

    ((_ (idx start stop step) e1 e2 ...)
     (let ((f (lambda (idx) e1 e2 ...)))
       (let loop ((m start))
	 (if (> m stop) #f
	     (begin (f m) (loop (+ m step)))))))))
(export repeat-with-index)


;; --<GUILE COMMENT>---------------------------------------------
;; <method name='tabulate'>
;;  <brief>Call a block of code a certain number of times, collecting
;;         the results in a list.</brief>
;;  <prototype> (tabulate n c)</prototype>
;;  <example> (use-modules (coasim batch))
;; (let* ((no-iterations 10000)
;;        (branch-lengths
;;         (tabulate no-iterations
;;          (let* ((m (snp-marker 0.5 0 1))
;;                 (arg (simulate (list m) 10))
;;                 (tree (car (local-trees arg)))
;;                 (branch-length (total-branch-length tree)))
;;            branch-length))))
;;   (display (/ (apply + branch-lengths) no-iterations))(newline)) </example>
;;  <description>
;;   <p>Execute the code `c' `n' number of times, collecting the
;;    results of evaluating `c' in a list that is returned as the
;;    result of the tabulation.
;;   </p>
;;  </description>
;; </method>
;; -----</GUILE COMMENT>----------------------------------------- 
(define-syntax tabulate
  (syntax-rules ()
    ((_ n e1 e2 ...)
       (let loop ((m n)
		  (acc '()))
	 (if (= m 0) (reverse acc)
	     (let* ((x (begin e1 e2 ...)) 
		    (new-acc (cons x acc)))
	       (loop (- m 1) new-acc)))))))
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
    ((_ n comb init e1 e2 ...)
     (let loop ((m n) (acc init))
       (if (= m 0) acc
	   (let* ((x (begin e1 e2 ...)) 
		  (new-acc (comb x acc)))
	     (loop (- m 1) new-acc)))))))
(export fold)
  


;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
