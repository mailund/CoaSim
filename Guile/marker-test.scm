
(define tm (trait-marker 0.1 0.18 0.22))
(define sm (snp-marker   0.2 0.10 0.90))
(define mm (ms-marker    0.3 0.4 4))

(display tm)(newline)
(display sm)(newline)
(display mm)(newline)

(display (position tm))(newline)
(display (position sm))(newline)
(display (position mm))(newline)


(display (trait-marker? tm))(display (snp-marker? tm))(display (ms-marker? tm))
(newline)
(display (trait-marker? sm))(display (snp-marker? sm))(display (ms-marker? sm))
(newline)
(display (trait-marker? mm))(display (snp-marker? mm))(display (ms-marker? mm))
(newline)
