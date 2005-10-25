
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim stepwise)">
;;  <brief>
;;    This module contains a step-wise mutation model for
;;    micro-satellite markers.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:

(define-module (coasim stepwise) 
  :use-module ((coasim) :select (custom-marker event-time))
  :use-module ((ice-9 optargs) :select (let-keywords)))

(define-public (step-ms-marker pos theta . kwargs)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='step-ms-marker'>
    <brief>A step-wise mutation model marker.</brief>
    <prototype>(step-ms-marker position theta)</prototype>
    <example>(step-ms-marker 0.1 1.5)</example>
    <description>
     <p>
      Creates a marker, at position pos, implementing the step-wise
      mutation model, i.e. whenever the marker mutates (and it does
      with rate theta) it increases or decreases by one with equal
      probability.
     </p>
     <p>
      The ancestral value of the marker is 0, but this can be changed 
      using the key-word parameter <b>:initial-value</b>.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  (let-keywords kwargs #f ((initial-value 0))
    (let* ((msec (cdr (gettimeofday)))
	   (random-state (seed->random-state msec))
	   
	   (waiting-time
	    (lambda ()
	      (let ((mean (/ 2 theta)));mean is 1/i where the 
					;intensity i is theta/2
		(* mean (random:exp random-state)))))
	   
	   (mutate-to
	    (lambda (parent-allele)
	      (if (< (random 1.0 random-state) 0.5)
		  (- parent-allele 1)
		  (+ parent-allele 1))))
	   
	   (mutate
	    (lambda (parent child parent-allele)
	      (let loop ((allele parent-allele)
			 (time-left (- (- (event-time parent) (event-time child))
				       (waiting-time))))
		(if (< time-left 0)
		    allele
		    (let ((new-allele (mutate-to allele))
			  (next-time (waiting-time)))
		      (loop new-allele (- time-left next-time))))))))
      
      (custom-marker pos initial-value mutate))))

;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
