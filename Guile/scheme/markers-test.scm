
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
  
(use-modules (srfi srfi-1))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (is-trait? (lambda (a) (= a 1)))
       (no-traits (length (filter (lambda (h) (is-trait? (car h))) seqs)))
       (cases-controls (split-in-cases-controls seqs 0 is-trait?))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls seqs 0 is-trait? 
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

(use-modules ((coasim SNP haplotypes) :select (split-in-cases-controls)))
(let* ((markers (make-random-snp-markers 10 0.1 0.9))
       (seqs    (simulate-sequences markers 10))
       (is-trait? (lambda (a) (= a 1)))
       (no-traits (length (filter (lambda (h) (is-trait? (car h))) seqs)))
       (cases-controls (split-in-cases-controls seqs 0))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls seqs 0 :remove-trait #f))
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
       (cases-controls (split-in-cases-controls seqs 0
						:disease-model 'recessive))
       (cases (car cases-controls))
       (controls (cadr cases-controls))

       (cases-controls2 (split-in-cases-controls seqs 0 
						 :disease-model 'recessive
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
