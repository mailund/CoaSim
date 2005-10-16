
(use-modules (coasim markers))

(define markers
  (sort-markers (list (snp-marker 0.1 0.1 0.9)
		      (snp-marker 0.3 0.1 0.9)
		      (snp-marker 0.2 0.1 0.9))))

(display markers)(newline)
(newline)

(define m-0.05 (trait-marker 0.05 0.1 0.2))
(define m-0.15 (trait-marker 0.15 0.1 0.2))
(define m-0.25 (trait-marker 0.25 0.1 0.2))
(define m-0.35 (trait-marker 0.35 0.1 0.2))

(display (insert-sorted-idx markers m-0.05))(newline)
(display (insert-sorted     markers m-0.05))(newline)
(newline)

(display (insert-sorted-idx markers m-0.15))(newline)
(display (insert-sorted     markers m-0.15))(newline)
(newline)

(display (insert-sorted-idx markers m-0.25))(newline)
(display (insert-sorted     markers m-0.25))(newline)
(newline)

(display (insert-sorted-idx markers m-0.35))(newline)
(display (insert-sorted     markers m-0.35))(newline)
(newline)


(define m-0.2 (trait-marker 0.2 0.1 0.2))
(catch 'position-occupied
       (lambda () (insert-sorted-idx markers m-0.2))
       (lambda (key . args) (display key)(newline)))

(catch 'position-occupied
       (lambda () (insert-sorted markers m-0.2))
       (lambda (key . args) (display key)(newline)))


(define mlist1 (list (snp-marker 0.0  0 1) (snp-marker 0.2  0 1)))
(define mlist2 (list (snp-marker 0.1  0 1) (snp-marker 0.4  0 1)))
(define mlist3 (list (snp-marker 0.12 0 1) (snp-marker 0.14 0 1)))

(display (merge-markers mlist1))(newline)
(display (merge-markers mlist1 mlist2))(newline)
(display (merge-markers mlist1 mlist3))(newline)
(display (merge-markers mlist1 mlist2 mlist3))(newline)
(newline)

(use-modules (coasim rand))
(let* ((snp-markers   (make-random-snp-markers   10 0.1 0.9))
       (trait-markers (make-random-trait-markers  2 0.2 0.4))
       (unsorted-markers (append snp-markers trait-markers))
       (sorted-markers (sort-markers unsorted-markers))
       (merged-markers (merge-markers snp-markers trait-markers)))
  (display (equal? unsorted-markers sort-markers))(newline)
  (display (equal? unsorted-markers merged-markers))(newline)
  (display (equal? sorted-markers merged-markers))(newline))
  
(use-modules (srfi srfi-1) (coasim disease-modelling))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (is-trait? (lambda (a) (= a 1)))
       (no-traits (length (filter (lambda (h) (is-trait? (car h))) seqs)))
       (cases-controls (split-in-cases-controls-on-marker seqs 0 is-trait?))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls-on-marker seqs 0 is-trait? 
							   :remove-trait #f))
       (cases2 (car cases-controls2))
       (controls2 (cadr cases-controls2))

       )
  (newline)
  (display no-traits)(newline)
  (display (length cases))(newline)
  (display (length cases2))(newline)
  (newline)
  (display (length (car cases)))(newline)
  (display (length (car cases2)))(newline)
  (newline)
  )

(use-modules ((coasim SNP haplotypes) :select (split-in-cases-controls-on-marker)))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (is-trait? (lambda (a) (= a 1)))
       (no-traits (length (filter (lambda (h) (is-trait? (car h))) seqs)))
       (cases-controls (split-in-cases-controls-on-marker seqs 0))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls-on-marker seqs 0 :remove-trait #f))
       (cases2 (car cases-controls2))
       (controls2 (cadr cases-controls2))

       )
  (newline)
  (display no-traits)(newline)
  (display (length cases))(newline)
  (display (length cases2))(newline)
  (newline)
  (display (length (car cases)))(newline)
  (display (length (car cases2)))(newline)
  (newline)
  )


(use-modules ((coasim SNP genotypes)))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (haplotypes->genotypes (simulate-sequences markers 10)))
       (is-trait? (lambda (a) (or (= a 1) (= a 2))))
       (no-traits (length (filter (lambda (h) (is-trait? (car h))) seqs)))
       (cases-controls (split-in-cases-controls-on-marker seqs 0
						:disease-model 'dominant))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls-on-marker seqs 0 
						 :disease-model 'dominant
						 :remove-trait #f))
       (cases2 (car cases-controls2))
       (controls2 (cadr cases-controls2))

       )
  (newline)
  (display no-traits)(newline)
  (display (length cases))(newline)
  (display (length cases2))(newline)
  (newline)
  (display (length (car cases)))(newline)
  (display (length (car cases2)))(newline)
  (newline)
  )

(use-modules ((coasim SNP genotypes)
	      :select (split-in-cases-controls-on-marker
		       split-in-cases-controls-on-marker/phased
		       pair-haplotypes)))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))

       (is-he? (lambda (a) (list? (member a '((0 1) (1 0))))))
       (is-hm? (lambda (a) (equal? a '(1 1))))
       (is-trait? (lambda (a) (or (is-he? a) (is-hm? a))))
       (no-traits 
	(let* ((genotypes (pair-haplotypes seqs))
	       (affected-genotype? (lambda (h) (is-trait? (car h))))
	       (affected-genotypes (filter affected-genotype? genotypes))
	       (no-aff-genotypes (length affected-genotypes)))
	  (* 2 no-aff-genotypes)))

       (cases-controls (split-in-cases-controls-on-marker/phased seqs 0
						:disease-model 'dominant))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls-on-marker/phased seqs 0 
						 :disease-model 'dominant
						 :remove-trait #f))
       (cases2 (car cases-controls2))
       (controls2 (cadr cases-controls2))
       )
  (newline)
  (display no-traits)(newline)
  (display (length cases))(newline)
  (display (length cases2))(newline)
  (newline)
  (display (length (car cases)))(newline)
  (display (length (car cases2)))(newline)
  (newline)
  )


(use-modules ((coasim disease-modelling)))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (is-trait? (lambda (a0 a1) (and (= a0 1) (= a1 1))))
       (cases-controls 
	(split-in-cases-controls-on-markers seqs '(0 1) is-trait?
					    :remove-traits #f))
       (cases (car cases-controls))
       (controls (cadr cases-controls))
       )
  (newline)
  (display "cases: ")(display cases)(newline)
  (newline)
  )

(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (qtl     (qtl-on-markers seqs '(0 1) + :remove-traits #f))
       )
  (newline)
  (display "qtl: ")(display qtl)(newline)
  (display "splitted<below>: ")(display (car (split-on-threshold qtl 1.5)))(newline)
  (display "splitted<above>: ")(display (cadr (split-on-threshold qtl 1.5)))(newline)
  (newline)
  )


(define f
  (let ((h (make-hash-table 4)))
    (hash-create-handle! h '(0 0)  0)
    (hash-create-handle! h '(0 1)  0)
    (hash-create-handle! h '(1 0) .1)
    (hash-create-handle! h '(1 1) .5)
    (table->function h)))

(define f
  (let ((t
	 (acons '(0 0)  0
	 (acons '(0 1)  0
	 (acons '(1 0) .1
	 (acons '(1 1) .5
	 '()))))))
    (table->function t)))


(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (qtl     (qtl-on-markers seqs '(0 1) f :remove-traits #f))
       (cases-controls (split-on-probability qtl))
       (cases (car cases-controls))
       (controls (cadr cases-controls))
       )
  (newline)
  (display "qtl: ")(display qtl)(newline)
  (display "cases>:    ")(display cases)(newline)
  (display "controls>: ")(display controls)(newline)
  (newline)
  )


