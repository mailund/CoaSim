
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating a coalescence tree and printing it to stdout.

(let* ((m (snp-marker 0.5 0 1))
       (arg (simulate (list m) 10))
       (tree (car (local-trees arg))))
  (display tree))
