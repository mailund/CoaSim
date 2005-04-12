
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating a coalescence tree and the local interval
;;; around it, and printing it to stdout.

(let* ((m (snp-marker 0.5 0 1))
       (arg (simulate (list m) 10 :rho 40))
       (i (car (intervals arg)))
       (t (interval->tree i)))
  (display (interval-start i))(display "-")
  (display (interval-end i))(display ": ")
  (display t))
