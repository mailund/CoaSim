
(define ARG (simulate '()
		      '(population 1
		         (merge 1.5
			      (population first  1 (sample 4))
			      (population second 1 (sample 4))))
		      :migration '((migration first second 1)
				   (migration second first 1))
		      :keep-empty-intervals #t
		      :random-seed 10))

(define mig-counts
  (fold-nodes ARG
	      (lambda (node count)
		(if (migration-node? node) (+ count 1) count))
	      0))

(display mig-counts)(newline) ;; should be 0


(define ARG (simulate '()
		      '(population 1
		         (merge 1.5
			      (population first  1 (sample 4))
			      (population second 1 (sample 4))))
		      :migration '((migration first second 1)
				   (migration second first 1))
		      :keep-empty-intervals #t
		      :keep-migration-events #t
		      :random-seed 10))

(define mig-counts
  (fold-nodes ARG
	      (lambda (node count)
		(if (migration-node? node) (+ count 1) count))
	      0))

(display mig-counts)(newline) ;; should be >0
