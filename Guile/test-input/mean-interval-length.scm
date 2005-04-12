
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Example of an ARG simulation from which we extract the local
;;; intervals and calculate the mean length.

(let* ((ARG (simulate '() 10 :rho 40 :keep-empty-intervals #t))
       (inter (intervals ARG))
       (len (lambda (i) (- (interval-end i) (interval-start i))))
       (interval-lengths (map len inter))
       (mean (/ (apply + interval-lengths) (length interval-lengths))))
  (display mean)(newline))
