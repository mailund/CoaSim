
(define p (arg-parameters 50 0 0 0))
(define markers
  (list (snp-marker   0.1 0.1 0.9)
	(snp-marker   0.2 0.1 0.9)
	(trait-marker 0.3 0.18 0.22)
	(ms-marker    0.4 0.5 4)
	(snp-marker   0.6 0.1 0.9)))

(define arg (simulate p markers 10 :random-seed 100))

(save-sequences arg "seq.out")
(define seqs (sequences arg))
(display seqs)(newline)
(newline)

(define retired-intervals (intervals arg))
(define start-positions (map interval-start retired-intervals))
(define end-positions (map interval-end retired-intervals))
(define branch-lengths (map total-branch-length retired-intervals))
(display branch-lengths)(newline)
(newline)

(define no-recomb (no-recombinations arg))
(display no-recomb)(newline)
(define no-coal (no-coalescence-events arg))
(display no-coal)(newline)
(define no-gc (no-gene-conversions arg))
(display no-gc)(newline)
(newline)

(define (print x) (display x)(display " "))
(define (combine f g) (lambda (x) (f (g x))))
(map (combine print total-branch-length) (intervals arg))(newline)
(map (combine print total-branch-length) (local-trees arg))(newline)
(newline)

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

(define overlapping-markers
  (list (snp-marker 0 0.1 0.9) (snp-marker 0 0.1 0.9)))
(catch 'out-of-sequence
       (lambda () (simulate p overlapping-markers 10))
       (lambda (key . args) (display key)(display " ")(display args)(newline)))

(define out-of-sequence-markers
  (list (snp-marker 0.1 0.1 0.9) (snp-marker 0.0 0.1 0.9)))
(catch 'out-of-sequence
       (lambda () (simulate p out-of-sequence-markers 10))
       (lambda (key . args) (display key)(display " ")(display args)(newline)))
