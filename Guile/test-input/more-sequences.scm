
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating a set of sequences.

(use-modules (coasim rand) (coasim io) (coasim batch))

(repeat-with-index (i 10)
   (let* ((markers (make-random-snp-markers 10 0 1))
	  (seqs (simulate-sequences markers 10))
	  (pos-file (string-append "positions." (number->string i) ".txt"))
	  (seq-file (string-append "sequences." (number->string i) ".txt")))
     (call-with-output-file pos-file (marker-positions-printer markers))
     (call-with-output-file seq-file (sequences-printer seqs))))



