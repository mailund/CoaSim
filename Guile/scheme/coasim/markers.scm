
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
  :use-module ((coasim) :select (position))
  :use-module ((ice-9 optargs) :select (let-keywords))
  :use-module ((srfi srfi-1) :select (take drop)))

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


(define-public (rescale-positions positions recombination-rates)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='rescale-positions'>
    <brief>Rescales marker positions to simulate variable recombination rate.</brief>
    <prototype>(rescale-positions positions recombination-rates)</prototype>
    <example> (use-modules (coasim markers))
 (define positions '(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
 (define recomb-rates '((0.5 40) (0.25 400) (0.25 40)))
 (define new-positions (rescale-positions positions recomb-rates))</example>
    <description>
     <p>
      Rescales the positions in `positions' according to `recombination-rates',
      a list of pairs where the first element is a lenght (of the 0-1 interval)
      and the second is the recombination rate over that range.  (The actual
      value of the rate is not important, only the relative differences
      between the rates in the list).
     </p>
     <p>
      The `recombination-rates' list is translated into a piece-wise linear
      function, where the different regions of the 0-1 interval specified by
      the distances in the list have different slope, and `positions' are then
      translated by this function by adding the sum of scaled distances before
      the region a position is in and multiplying the remainder distance from 
      the start of the piece to the position by the recombination rate of the
      piece:
     </p>
     <center><em>new_pos = (pos-last_break)*a + b</em></center>
     <p>
      Where <em>last_break<em> is the beginning of the piece containing 
      <em>pos</em>, <em>a</em> is the rate of the piece, and <em>b</em> is the
      sum of distances times rates leading up to this piece.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  ;; sanity check
  (let ((distance-sum (apply + (map car recombination-rates))))
    (if (not (= 1.0 distance-sum))
	(throw 'illegal-distances distance-sum)))

  (let* (;; distances in the 0-1 interval
	 (distances (map car recombination-rates))
	 ;; the different rates
	 (rates (map cadr recombination-rates))
	 
	 ;; breaks between different rates
	 (break-points
	  (let loop ((bp '(0)) (dists distances))
	    (if (null? dists) (cdr (reverse bp)) ; reverse order, remove 0
		(loop (cons (+ (car dists) (car bp)) bp)
		      (cdr dists)))))

	 ;; break-points scaled wrt rates
	 (reversed-scaled-break-points
	  (let loop ((bp '(0)) (dists distances) (rs rates))
	    (if (null? dists) bp
		(loop (cons (+ (* (car rs) (car dists)) (car bp)) bp)
		      (cdr dists)
		      (cdr rs)))))

	 ;; max scaled break point, used for back-scaling
	 (end-point (car reversed-scaled-break-points))
	 ;; function for scaling back to 0-1 interval
	 (back-scale (lambda (x) (/ x end-point)))

	 ;; linear functions used for re-scaling
	 (as rates)
	 (bs (reverse (cdr reversed-scaled-break-points)))
	 (gen-lin-fun (lambda (a b) (lambda (x) (+ (* a x) b))))
	 (lin-funs (map gen-lin-fun as bs))

	 ;; scale positions wrt linear functions
	 (scaled-positions
	  (let loop ((pos positions)
		     (scaled-pos '())
		     (bps break-points)
		     (last-break 0)
		     (lfs lin-funs))
	    (cond ((null? pos) (reverse scaled-pos))
		  ((> (car pos) (car bps))
		   (loop pos scaled-pos (cdr bps) (car bps) (cdr lfs)))
		  (else
		   (loop (cdr pos) 
			 (cons ((car lfs) (- (car pos) last-break)) scaled-pos)
			 bps last-break lfs))))))

    (map back-scale scaled-positions)))




(define-public (remove-marker seqs idx)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='remove-marker'>
    <brief>Removes the alleles of the marker at `idx' from the sequences.</brief>
    <prototype>(remove-marker sequences idx)</prototype>
    <example> (use-modules (coasim markers))
 (remove-marker seqs trait-idx)))</example>
    <description>
     <p>
      Removes the alleles at `idx' from the sequences; useful for example for
      removing a trait marker from a dataset.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  (letrec ((strip (lambda (h)
		    (let ((first (take h idx))
			  (rest  (drop h (+ 1 idx))))
		      (append first rest)))))
    (map strip seqs)))


(define-public (split-in-cases-controls haplotypes trait-idx is-case? . kwargs)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='split-in-cases-controls'>
    <brief>Split a list of sequences into cases and controls.</brief>
    <prototype>(split-in-cases-controls sequences trait-idx is-case?)</prototype>
    <example>(use-modules (coasim markers))
    (let ((is-case? (lambda (a) (= 1 a))))
        (split-in-cases-controls seqs trait-idx is-case?)))</example>
    <description>
     <p>
      Split a dataset into cases and controls, based on the value at
      trait-idx.  If the value at that index satisfy the is-case? predicate,
      the sequence is considered a case, otherwise a control.
     </p>
     <p>
      By default the alleles at the trait marker is removed from the resulting
      lists; an optional parameter, :remove-trait, if set to #f, will prevent
      removal of the trait marker.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (let-keywords kwargs #f ((remove-trait #t))
   (letrec ((g (if remove-trait
		   (lambda (h)
		     (let ((first (take h trait-idx))
			   (rest  (drop h (+ 1 trait-idx))))
		       (append first rest)))
		   (lambda (h) h)))

	   (f (lambda (lst cases controls)
		(cond ((null? lst) (list (reverse cases) (reverse controls)))
 		      ((is-case? (list-ref (car lst) trait-idx))
		       (f (cdr lst) 
			  (cons (g (car lst)) cases)
			  controls))
		      (else
		       (f (cdr lst) 
			  cases
			  (cons (g (car lst)) controls)))))))
    (f haplotypes '() '()))))


;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
