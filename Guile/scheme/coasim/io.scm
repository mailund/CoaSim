
;;; CoaSim -- A coalescence process simulator
;;;
;;; Copyright (C) 2004, 2005 by Bioinformatics ApS
;;;                             <URL:http://bioinformatics.dk>


;;; Commentary:

;; --<GUILE COMMENT>---------------------------------------------

;; <module name="(coasim io)">
;;  <brief>
;;    This module contains functions for reading and writing coasim data.
;;  </brief>

;; -----</GUILE COMMENT>----------------------------------------- 

;;; Code:


(define-module (coasim io) :use-module ((coasim) :select (position)))

(define (bind-1st f first)  (lambda (second) (f first second)))
(define (bind-2nd f second) (lambda (first)  (f first second)))

(define (print-list port lst)
  "Prints `lst' to `port'. "
  (let ((print-elm (lambda (elm) (display elm port)(display " " port))))
    (for-each print-elm lst)
    (newline port)))


;;; Sequence output
(define-public (print-sequences port sequences)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='print-sequences'>
    <brief>Print a list of sequences.</brief>
    <prototype>(print-sequences port sequences)</prototype>
    <example>(print-sequences (current-output-port) sequences)</example>
    <description>
     <p>
      This function prints the sequences in the list `sequences' to
      the output port `port'.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
  "
  (for-each (bind-1st print-list port) sequences))

;;; Position output
(define-public (print-positions port positions)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='print-positions'>
    <brief>Print a list of positions.</brief>
    <prototype>(print-positions port positions)</prototype>
    <example>(print-positions (current-output-port) positions)</example>
    <description>
     <p>
      This function prints the positions (numbers) in the list `positions' to
      the output port `port'.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
  (print-list port positions))

(define-public (print-marker-positions port markers)
  "
   --<GUILE COMMENT>---------------------------------------------
   <method name='print-marker-positions'>
    <brief>Print a list of positions from markers.</brief>
    <prototype>(print-marker-positions port markers)</prototype>
    <example>(print-marker-positions (current-output-port) markers)</example>
    <description>
     <p>
      This function prints the positions of the markers in the list `markers'
      to the output port `port'.
     </p>
    </description>
   </method>
   -----</GUILE COMMENT>----------------------------------------- 
   "
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


;; --<GUILE COMMENT>---------------------------------------------

;; <method name='sequences-printer'>
;;  <brief>Convenience function for printing sequences.</brief>
;;  <prototype>(sequences-printer sequences)</prototype>
;;  <example>(call-with-output-file "sequences.txt" (sequences-printer sequences))</example>
;;  <description>
;;   <p>
;;    A convenience function, based on `print-sequences', that, given a 
;;    list of sequences, gives a function with one argument, a port, that
;;    when applied writes the sequences to the port.
;;   </p>
;;   <p>
;;    The function is useful for calls to `call-with-output-file'.
;;   </p>
;;  </description>
;; </method>

;; <method name='sequences-port'>
;;  <brief>Convenience function for printing sequences.</brief>
;;  <prototype>(sequences-port port)</prototype>
;;  <example>(map (sequence-port port) list-of-sequences)</example>
;;  <description>
;;   <p>
;;    A convenience function, based on `print-sequences', that, given a 
;;    list a port, gives a function with one argument, a sequence, that
;;    when applied writes the sequences to the port.
;;   </p>
;;   <p>
;;    The function is useful for mapping over lists of sequences.
;;   </p>
;;  </description>
;; </method>



;; <method name='positions-printer'>
;;  <brief>Convenience function for printing positions.</brief>
;;  <prototype>(positions-printer positions)</prototype>
;;  <example>(call-with-output-file "positions.txt" (positions-printer positions))</example>
;;  <description>
;;   <p>
;;    A convenience function, based on `print-positions', that, given a 
;;    list of positions, gives a function with one argument, a port, that
;;    when applied writes the positions to the port.
;;   </p>
;;   <p>
;;    The function is useful for calls to `call-with-output-file'.
;;   </p>
;;  </description>
;; </method>

;; <method name='positions-port'>
;;  <brief>Convenience function for printing positions.</brief>
;;  <prototype>(positions-port port)</prototype>
;;  <example>(map (position-port port) list-of-positions)</example>
;;  <description>
;;   <p>
;;    A convenience function, based on `print-positions', that, given a 
;;    list a port, gives a function with one argument, a position, that
;;    when applied writes the positions to the port.
;;   </p>
;;   <p>
;;    The function is useful for mapping over lists of positions.
;;   </p>
;;  </description>
;; </method>



;; <method name='marker-positions-printer'>
;;  <brief>Convenience function for printing marker-positions.</brief>
;;  <prototype>(marker-positions-printer marker-positions)</prototype>
;;  <example>(call-with-output-file "marker-positions.txt" (marker-positions-printer marker-positions))</example>
;;  <description>
;;   <p>
;;    A convenience function, based on `print-marker-positions', that, given a 
;;    list of marker-positions, gives a function with one argument, a port, that
;;    when applied writes the marker-positions to the port.
;;   </p>
;;   <p>
;;    The function is useful for calls to `call-with-output-file'.
;;   </p>
;;  </description>
;; </method>

;; <method name='marker-positions-port'>
;;  <brief>Convenience function for printing marker-positions.</brief>
;;  <prototype>(marker-positions-port port)</prototype>
;;  <example>(map (position-port port) list-of-marker-positions)</example>
;;  <description>
;;   <p>
;;    A convenience function, based on `print-marker-positions', that, given a 
;;    list a port, gives a function with one argument, a position, that
;;    when applied writes the marker-positions to the port.
;;   </p>
;;   <p>
;;    The function is useful for mapping over lists of marker-positions.
;;   </p>
;;  </description>
;; </method>

;; -----</GUILE COMMENT>----------------------------------------- 

(make-printer-and-port print-sequences)
(make-printer-and-port print-positions)
(make-printer-and-port print-marker-positions)

;; --<GUILE COMMENT>---------------------------------
;; </module>
;; --</GUILE COMMENT>--------------------------------
