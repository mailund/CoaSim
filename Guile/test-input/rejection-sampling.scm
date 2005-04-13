
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of sampling genealogies without recombination, from
;;; simulations with a recombination rate rho>0, using callbacks and
;;; rejection sampling.


(define (try-simulate)
  ;; keep trying to simulate until a simulation is NOT aborted
  (let* ((reject-recomb (lambda (n1 n2 k) (throw 'reject)))
	 (try (lambda ()
		;; simulate an ARG, but reject the simulation if a
		;; recombination occurred
		(simulate '() 10 :rho 5
			  :recombination-callback reject-recomb
			  :keep-empty-intervals   #t)))
	 (except (lambda (ex-key . ex-args) #f)))
    (let ((res (catch 'reject try except)))
      (if res res (try-simulate)))))

(define no-iterations 5)
(use-modules (coasim batch))
(let ((sum-of-heights
       (fold 5 (lambda (val sum) (+ val sum)) 0
	     (tree-height (car (local-trees (try-simulate)))))))
  (display (/ sum-of-heights no-iterations))
  (newline))


