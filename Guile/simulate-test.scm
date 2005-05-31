
(define markers
  (list (snp-marker   0.1 0.1 0.9)
	(snp-marker   0.2 0.1 0.9)
	(trait-marker 0.3 0.18 0.22)
	(ms-marker    0.4 0.5 20)
	(snp-marker   0.6 0.1 0.9)))

(define arg (simulate markers 10 :rho 50 :random-seed 100))

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

(display (simulate-sequences markers 10    :rho 50 :random-seed 100))(newline)
(display (simulate-sequences markers 10 1  :rho 50 :random-seed 100))(newline)
(display (simulate-sequences markers '(10) :rho 50 :random-seed 100))(newline)
(display (simulate-sequences markers '(10) '(1) :rho 50 :random-seed 100))(newline)
(display (simulate-sequences markers '(10) 1 :rho 50 :random-seed 100))(newline)
(display (simulate-sequences markers 10 .1 :rho 50 :random-seed 100))(newline)

(newline)
(display (simulate-sequences markers '(10 10) '(.2 .9) 
                             :epochs (list (population-merge 0 1 0.2))
                             :random-seed 100))
(newline)


(newline)
(display "now testing validation of incorrect input\n")
(display "this will print some error messages\n")
(newline)

(catch 'wrong-number-of-args
       (lambda () (simulate))
       (lambda (key . args) (display key)(newline)))

(catch 'wrong-number-of-args
       (lambda () (simulate markers))
       (lambda (key . args) (display key)(newline)))

(catch #t
       (lambda () (simulate 0 10))
       (lambda (key . args) (display key)(newline)))

(define overlapping-markers
  (list (snp-marker 0 0.1 0.9) (snp-marker 0 0.1 0.9)))
(catch 'out-of-sequence
       (lambda () (simulate overlapping-markers 10))
       (lambda (key . args) (display key)(display " ")(display args)(newline)))

(define out-of-sequence-markers
  (list (snp-marker 0.1 0.1 0.9) (snp-marker 0.0 0.1 0.9)))
(catch 'out-of-sequence
       (lambda () (simulate out-of-sequence-markers 10))
       (lambda (key . args) (display key)(display " ")(display args)(newline)))

(catch 'non-positive-population-size
       (lambda () (simulate '() -10))
       (lambda (key . args) (display key)(display " ")(display args)(newline)))
