
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
