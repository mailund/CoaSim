
(use-modules (ice-9 format) (coasim batch))

(repeat 2
 (let* ((m (snp-marker 0.5 0 1))
	(arg (simulate (list m) 5))
	(tree (car (local-trees arg))))
   (display tree)))
(newline)

(repeat-while (lambda (branch-length) (< branch-length 5))
 (let* ((m (snp-marker 0.5 0 1))
	(arg (simulate (list m) 10))
	(tree (car (local-trees arg)))
	(branch-length (total-branch-length tree)))
   (format #t "~4f " branch-length)
   branch-length))
(newline)


(let* ((no-iterations 10)
       (branch-lengths
	(tabulate no-iterations
		  (let* ((m (snp-marker 0.5 0 1))
			 (arg (simulate (list m) 10))
			 (tree (car (local-trees arg)))
			 (branch-length (total-branch-length tree)))
		    branch-length))))
  (display (/ (apply + branch-lengths) no-iterations))(newline))


(define betas '(0 10 20))
(define (mean-branch-length beta)
  (let* ((no-iterations 1000)
	 (branch-lengths
	  (tabulate no-iterations
	    (let ((arg (simulate (list (snp-marker 0.5 0 1)) 10 :beta beta)))
	      (total-branch-length (car (local-trees arg)))))))
    (/ (apply + branch-lengths) no-iterations)))

(display (map mean-branch-length betas))(newline)
(newline)

(let* ((no-iterations 10)
       (branch-sum
	(fold no-iterations (lambda (val sum) (+ val sum)) 0
	 (let* ((m (snp-marker 0.5 0 1))
		(arg (simulate (list m) 10))
		(tree (car (local-trees arg)))
		(branch-length (total-branch-length tree)))
	   branch-length))))
  (display (/ branch-sum no-iterations))(newline))

;(use-modules (ice-9 syncase))
;(define-syntax repeat-with-index
;  (syntax-rules ()
;    ((_ (idx n) e1 e2 ...)
;     (let ((f (lambda (idx) e1 e2 ...)))
;       (let loop ((m 1))
;	 (if (> m n) #f
;	     (begin (f m) (loop (+ m 1)))))))
;
;    ((_ (idx start stop) e1 e2 ...)
;     (let ((f (lambda (idx) e1 e2 ...)))
;       (let loop ((m start))
;	 (if (> m stop) #f
;	     (begin (f m) (loop (+ m 1)))))))
;
;    ((_ (idx start stop step) e1 e2 ...)
;     (let ((f (lambda (idx) e1 e2 ...)))
;       (let loop ((m start))
;	 (if (> m stop) #f
;	     (begin (f m) (loop (+ m step)))))))))

(repeat-with-index (i 3) (display i)(newline))
(newline)
(repeat-with-index (i 2 6) (display i)(newline))
(newline)
(repeat-with-index (i 1 10 2) (display i)(newline))
(newline)
