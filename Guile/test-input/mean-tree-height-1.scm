
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>

;;; Example of simulating an ARG and calculating the mean tree height
;;; for the local trees.

(let* ((ARG (simulate '() 10 :rho 40 :beta 10 :keep-empty-intervals #t))
       (trees (local-trees ARG))
       (tree-heights (map tree-height trees))
       (mean (/ (apply + tree-heights) (length tree-heights))))
  (display mean)(newline))
