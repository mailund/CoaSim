
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Commentary:

;;; This module contains functions making random marker configurations.

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

(define (make-marker constructor)
  "General random marker constructor."
  (lambda (no-markers  first-arg second-arg)
    (let ((positions (make-random-positions no-markers))
	  (f (lambda (pos) (constructor pos first-arg second-arg))))
      (map f positions))))

(define-public make-random-trait-markers (make-marker trait-marker))
(define-public make-random-snp-markers   (make-marker snp-marker))
(define-public make-random-ms-markers    (make-marker ms-marker))
