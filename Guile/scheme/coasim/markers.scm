
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Commentary:

;;; This module contains functions manipulating sorted lists of markers.

;;; Code:

(define-module (coasim markers) 
  :use-module ((coasim) :select (position)))

(define-public (sort-markers markers)
  "Sorts a list of markers wrt. their position."
  (sort markers (lambda (m1 m2) (< (position m1) (position m2)))))


(define-public (insert-sorted-idx sorted-list marker)
  "This function inserts a marker into a sorted list of markers and
return a list who's first element is the resulting list and who's
second element is the index the marker got in the new list."
  (letrec ((p (position marker))
	   (f (lambda (lst i c)
		(cond ((null? lst) (list (c (list marker)) i))
		      ((= p (position (car lst))) 
		       (throw 'position-occupied))
		      ((< p (position (car lst)))
		       (list (c (cons marker lst)) i))
		      (else
		       (f (cdr lst) (+ i 1)
			  (lambda (tail) (c (cons (car lst) tail)))))))))
    (f sorted-list 0 (lambda (lst) lst))))


(define-public (insert-sorted sorted-list marker)
  "This function inserts a marker into a sorted list of markers and
return the resulting list."
  (let ((list-and-index (insert-sorted-idx sorted-list marker)))
    (car list-and-index)))
