#!/usr/bin/env --split-string=guile -s
!#
(use-modules (ice-9 textual-ports))
(define file (open-input-file "example.txt"))
(define (readfile)
  (let
    ((line (get-line file)))
    (cond
      ((eof-object? line) '())
      (else (cons line (readfile))))))

(define contents (readfile))
(define numbers (map string->number contents))

(define (zip l1 l2) (map list l1 l2))
(define numbers-with-index
  (zip (iota (length numbers)) numbers))

; What a mess, no built-in functions in Guile to find or delete the nth element
; in a list? This is completely unusable ...
; Let's switch to to something else.
(define (move-at list-before index)
  (splice in (cut)))

(display numbers)
(newline)
(display numbers-with-index)
(newline)
