
(define p (arg-parameters 50 0 0 0))
(define markers
  (list (snp-marker   0.1 0.1 0.9)
	(snp-marker   0.2 0.1 0.9)
	(trait-marker 0.3 0.18 0.22)
	(ms-marker    0.4 0.5 4)
	(snp-marker   0.6 0.1 0.9)))

(define arg (simulate p markers 10))

(save-sequences arg "seq.out")

(catch 'wrong-number-of-args
       (lambda () (simulate p))
       (lambda (key . args) (display key)(newline)))

(catch 'wrong-number-of-args
       (lambda () (simulate p markers))
       (lambda (key . args) (display key)(newline)))

(catch 'wrong-type-arg
       (lambda () (simulate 0 markers 10))
       (lambda (key . args) (display key)(newline)))

(catch #t
       (lambda () (simulate p 0 10))
       (lambda (key . args) (display key)(newline)))

