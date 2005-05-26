
(display (bottleneck 0.1 1 2))(newline)

(catch 'illegal-epoch
       (lambda () (bottleneck 0.1 2 1))
       (lambda (key . args) (display key)(newline)))

