
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim markers)">
;;  <brief>
;;    This module contains functions manipulating sorted lists of markers.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:

(define-module (coasim markers) 
  :use-module ((coasim) :select (position)))

(define (cmp-markers m1 m2) (< (position m1) (position m2)))

(define-public (sort-markers markers)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='sort-markers'>
    <brief>Sort a list of markers.</brief>
    <prototype>(sort-markers marker-list)</prototype>
    <example>(sort-markers marker-list)</example>
    <description>
     <p>
      Sort a list of markers with relation to their position.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  (sort markers cmp-markers))


(define-public (insert-sorted-idx sorted-list marker)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='insert-sorted-idx'>
    <brief>Inserts a marker in a sorted list of markers..</brief>
    <prototype>(insert-sorted-idx marker-list marker)</prototype>
    <example> (define snp-markers (make-random-snp-markers 10 0.1 0.9))
 (define disease-marker (car (make-random-trait-markers 1 0.18 0.22)))
 (define markers-and-index (insert-sorted-idx snp-markers disease-marker))</example>
    <description>
     <p>
      This function inserts a marker into a sorted list of markers and
      return a list who's first element is the resulting list and who's
      second element is the index the marker got in the new list.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
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
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='insert-sorted'>
    <brief>Inserts a marker in a sorted list of markers.</brief>
    <prototype>(insert-sorted sorted-marker-list marker)</prototype>
    <example>(insert-sorted sorted-marker-list marker)</example>
    <description>
     <p>
      This function inserts a marker into a sorted list of markers and
      return the resulting list.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  (let ((list-and-index (insert-sorted-idx sorted-list marker)))
    (car list-and-index)))

(define-public (merge-markers . list-of-marker-lists)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='merge-markers'>
    <brief>Merge two or more lists of sorted markers.</brief>
    <prototype>(merge-markers . list-of-marker-lists)</prototype>
    <example>(merge-markers marker-list-1 marker-list-2 marker-list-3)</example>
    <description>
     <p>
      Merge two or more lists of sorted markers.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  (if (null? list-of-marker-lists) '()
      (let loop ((merged (car list-of-marker-lists))
		 (rest   (cdr list-of-marker-lists)))
	(if (null? rest) merged
	    (loop (merge merged (car rest) cmp-markers) (cdr rest))))))



;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
