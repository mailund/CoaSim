
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Commentary:

;;; This module contains functions for manipulating SNP haplotypes.

;;; Code:

(define-module (coasim SNP haplotypes) 
  :use-module ((coasim SNP shared) 
	       :select ((split-in-cases-controls . split))))

(define-public (split-in-cases-controls haplotypes trait-idx)
  "Split a dataset into cases and controls, based on the value at
trait-idx."
  (let ((is-case? (lambda (h) (= 1 (list-ref h trait-idx)))))
    (split haplotypes trait-idx is-case?)))
