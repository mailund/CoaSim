
(use-modules (ice-9 format) (coasim batch))

(repeat 2
 (let* ((m (snp-marker 0.5 0 1))
	(arg (simulate (list m) 5))
	(tree (car (local-trees arg))))
   (display tree)))
(newline)

(define (f)
 (let* ((m (snp-marker 0.5 0 1))
	(arg (simulate (list m) 5))
	(tree (car (local-trees arg))))
   (display tree)))

(repeat 2 f)
(newline)


(repeat-while (lambda (branch-length) (< branch-length 5))
 (let* ((m (snp-marker 0.5 0 1))
	(arg (simulate (list m) 10))
	(tree (car (local-trees arg)))
	(branch-length (total-branch-length tree)))
   (format #t "~4f " branch-length)
   branch-length))
(newline)

(define (f)
  (let* ((m (snp-marker 0.5 0 1))
	 (arg (simulate (list m) 10))
	 (tree (car (local-trees arg)))
	 (branch-length (total-branch-length tree)))
    (format #t "~4f " branch-length)
    branch-length))
(define (p branch-length)
  (< branch-length 5))

(repeat-while p f)
(newline)

(repeat-while
 (let* ((m (snp-marker 0.5 0 1))
	(arg (simulate (list m) 10))
	(tree (car (local-trees arg)))
	(branch-length (total-branch-length tree)))
   (format #t "~4f " branch-length)
   (< branch-length 5)))
(newline)
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

(define (simulate-branch-length)
  (let* ((m (snp-marker 0.5 0 1))
	 (arg (simulate (list m) 10))
	 (tree (car (local-trees arg)))
	 (branch-length (total-branch-length tree)))
    branch-length))

(let* ((no-iterations 10)
       (branch-lengths (tabulate no-iterations simulate-branch-length)))
  (display (/ (apply + branch-lengths) no-iterations))(newline))


;; using transformer to translate arg into a branch-length
(let* ((no-iterations 10)
       (branch-lengths
	(tabulate no-iterations
		  (lambda (arg) (total-branch-length (car (local-trees arg))))
		  (simulate (list (snp-marker 0.5 0 1)) 10))))
  (display (/ (apply + branch-lengths) no-iterations))(newline))

;; using transformer to translate arg into a branch-length, and
;; a predicate to only include args with a single recombination
(let* ((no-iterations 10)
       (branch-lengths
	(tabulate no-iterations
		  (lambda (arg) (total-branch-length (car (local-trees arg))))
		  (lambda (arg) (= 1 (no-recombinations arg)))
		  (simulate (list (snp-marker 0.5 0 1)) 10 :rho 0.3))))
  (format #t "~d out of ~d ARGs had a single recombination.\n"
	  (length branch-lengths) no-iterations)
  (format #t "The average branch length was: ~4f.\n"
	  (/ (apply + branch-lengths) (length branch-lengths))))


(define betas '(0 10 20))
(define (mean-branch-length beta)
  (let* ((no-iterations 1000)
	 (simulate-branch-length
	  (lambda ()
	    (let ((arg (simulate (list (snp-marker 0.5 0 1)) 10 :beta beta)))
	      (total-branch-length (car (local-trees arg))))))
	 (branch-lengths
	  (tabulate no-iterations simulate-branch-length)))
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
