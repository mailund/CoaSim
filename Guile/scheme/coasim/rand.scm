
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>


;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim rand)">
;;  <brief>
;;    This module contains functions making random marker configurations.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:

(define-module (coasim rand)
  :use-module ((coasim)      :select (trait-marker snp-marker ms-marker))
  :use-module ((srfi srfi-1) :select (list-tabulate)))

(define (two-equal sorted-list)
  "Tests if there are two equal elements in a sorted list."
  (if (null? sorted-list) #f
      (letrec ((f (lambda (first rest)
		    (cond ((null? rest) #f)
			  ((equal? first (car rest)) #t)
			  (else (f (car rest) (cdr rest)))))))
	(f (car sorted-list) (cdr sorted-list)))))

(define (make-random-positions no-positions)
  "Make `no-positions' random, disjoint positions."
  (let* ((msec (cdr (gettimeofday)))
	 (random-state (seed->random-state msec))
	 (random-pos (lambda (dummy) (random 1.0 random-state)))
	 (pos (list-tabulate no-positions random-pos))
	 (sorted (sort pos <)))
    (if (two-equal sorted) (make-random-positions no-positions) ; retry
	sorted)))

(define (make-markers constructor)
  "General random marker constructor."
  (lambda (no-markers  first-arg second-arg)
    (let ((positions (make-random-positions no-markers))
	  (f (lambda (pos) (constructor pos first-arg second-arg))))
      (map f positions))))



;; --<GUILE COMMENT>---------------------------------------------

;; <method name='make-random-trait-markers'>
;;  <brief>Make a list of random trait markers.</brief>
;;  <prototype>(make-random-trait-markers no-markers low-freq high-freq)</prototype>
;;  <example>(define disease-marker (car (make-random-trait-markers 1 0.18 0.22)))</example>
;;  <description>
;;   <p>
;;    Make a list of random positioned trait markers.  The low and 
;;    high frequency constraints are similar to the `trait-marker'
;;    function.
;;   </p>
;;  </description>
;; </method>

;; <method name='make-random-snp-markers'>
;;  <brief>Make a list of random SNP markers.</brief>
;;  <prototype>(make-random-snp-markers no-markers low-freq high-freq)</prototype>
;;  <example>(define snp-markers (make-random-snp-markers 10 0.1 0.9))</example>
;;  <description>
;;   <p>
;;    Make a list of random positioned SNP markers.  The low and 
;;    high frequency constraints are similar to the `snp-marker'
;;    function.
;;   </p>
;;  </description>
;; </method>

;; <method name='make-random-ms-markers'>
;;  <brief>Make a list of random micro-satellite markers.</brief>
;;  <prototype>(make-random-ms-markers no-markers theta K)</prototype>
;;  <example>(define snp-markers (make-random-ms-markers 10 0.1 15))</example>
;;  <description>
;;   <p>
;;    Make a list of random positioned micro-satellite markers.
;;    The theta and K parameters are similar to the `ms-marker'
;;    function.
;;   </p>
;;  </description>
;; </method>

;; --</GUILE COMMENT>--------------------------------


(define-public make-random-trait-markers (make-markers trait-marker))
(define-public make-random-snp-markers   (make-markers snp-marker))
(define-public make-random-ms-markers    (make-markers ms-marker))


;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
