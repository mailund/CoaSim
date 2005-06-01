
(display (bottleneck 0 0.1 1 2))(newline)
(display (growth 0 10 1 2))(newline)
(display (migration 0 1 .98 0 1))(newline)
(display (population-merge 0 1 .98))(newline)

(catch 'illegal-epoch
       (lambda () (bottleneck 0 0.1 2 1))
       (lambda (key . args) (display key)(newline)))

(catch 'illegal-epoch
       (lambda () (bottleneck 0 -0.1 1 2))
       (lambda (key . args) (display key)(newline)))

