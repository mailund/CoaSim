
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004 by Bioinformatics ApS <URL:http://bioinformatics.dk>

;;; Commentary:

;;; This module contains functions for reading and writing coasim data.

;;; Code:


(define-module (coasim io) :use-module ((coasim) :select (position)))

(define (bind-1st f first)  (lambda (second) (f first second)))
(define (bind-2nd f second) (lambda (first)  (f first second)))

(define (print-list port lst)
  "Prints the list `lst' to `port'."
  (let ((print-elm (lambda (elm) (display elm port)(display " " port))))
    (for-each print-elm lst)
    (newline port)))


;;; Haplotype output
(define-public (print-haplotypes port haplotypes)
  "This function prints the haplotypes in the list `haplotypes' to the
output port `port'."
  (for-each (bind-1st print-list port) haplotypes))

;;; Position output
(define-public (print-positions port positions)
  "Prints a list of positions to a port."
  (print-list port positions))

(define-public (print-marker-positions port markers)
  "Print the positions for the `markers' to `port'."
  (print-positions port (map position markers)))


;; Makes convenience functions for making a -printer and a -port
;; version of each print- function.
(define-macro (make-printer-and-port func)
  (let* ((func-name (symbol->string func))
	 ;; remove "print-"
	 (name-str  (substring func-name 6 (string-length func-name)))
	 (port-name    (string->symbol (string-append name-str "-port")))
	 (printer-name (string->symbol (string-append name-str "-printer"))))
    `(begin 
       (define-public ,printer-name (lambda (data) (bind-2nd ,func data)))
       (define-public ,port-name    (lambda (port) (bind-1st ,func port))))))

(make-printer-and-port print-haplotypes)
(make-printer-and-port print-positions)
(make-printer-and-port print-marker-positions)
