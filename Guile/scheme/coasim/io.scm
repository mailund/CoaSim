
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Commentary:

;;; This module contains functions for reading and writing coasim data.

;;; Code:


(define-module (coasim io)
  :use-module ((srfi srfi-1)))

(define-public (print-haplotypes port haplotypes)
  "This function prints the haplotypes in the list `haplotypes' to the
output port `port'."
  (let ((print-haplotype 
	 (lambda (h) 
	   (for-each (lambda (a) (display a port)(display " " port)) h)
	   (newline port))))
    (for-each print-haplotype haplotypes)))

(define-public (haplotype-printer haplotypes)
  "This function is a convenience function for use with call-with-output-file.

Given a list of haplotypes, it creates a function that takes a port as
argument and writes the haplotypes to this port, thus it can be used
as:

  (call-with-output-file filename (haplotype-printer haplotypes))

which will write the `haplotypes' to `filename'."
  (lambda (port) (print-haplotypes port haplotypes)))
