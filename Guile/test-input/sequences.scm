
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating a list of sequences.

(use-modules (coasim rand) (coasim io))

(let* ((markers (make-random-snp-markers 10 0 1))
       (seqs (simulate-sequences markers 10)))
  (display seqs)(newline))

(let* ((markers (make-random-snp-markers 10 0 1))
       (seqs (simulate-sequences markers 10)))
  (call-with-output-file "positions.txt" (marker-positions-printer markers))
  (call-with-output-file "sequences.txt" (sequences-printer seqs)))



