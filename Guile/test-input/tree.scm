
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating a coalescence tree and printing it to stdout.

(let* ((ARG (simulate '() 10 :keep-empty-intervals #t))
       (tree (car (local-trees ARG))))
  (display tree))
