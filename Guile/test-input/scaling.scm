;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of calculating a re-scaling factor for genealogies with
;;; exponential groth, through simulations.

(use-modules (coasim batch))

(define no-leaves    20)

(define (calc-scale-factor beta)
  (if (= beta 0) (cons 0 1) ; no need to simulate this one...
      (let ((zero-growth-tree-length
	     ;; calculating the tree length without growth from closed
	     ;; term formula
	     (let loop ((j 1) (sum 0))
	       (if (< j no-leaves)
		   (loop (+ j 1) (+ sum (/ 1 j)))
		   sum)))
	    (growth-tree-length
	     ;; simulating the tree length with growth
	     (let* ((no-iterations 10000)
		    (branch-sum
		     (fold no-iterations (lambda (val sum) (+ val sum)) 0
			   (let* ((ARG (simulate '() no-leaves :beta beta
						 :keep-empty-intervals #t))
				  (tree (car (local-trees ARG))))
			     (total-branch-length tree)))))
	       (/ branch-sum no-iterations))))
	(cons beta (/ zero-growth-tree-length growth-tree-length)))))

(define betas '(0 10 100))
(define scale-factors (map calc-scale-factor betas))

(define (scaled-simulate beta rho theta)
  (let* ((scale-factor   (assoc-ref scale-factors beta))
	 (scaled-rho     (* scale-factor rho))
	 (scaled-theta   (* scale-factor theta))
	 )
    (display scale-factor)(newline)
    (display scaled-rho)(newline)
    (display scaled-theta)(newline)))

(scaled-simulate 10 40 1.3)
