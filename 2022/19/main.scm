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
(display (length contents))
(newline)
(display contents)
(newline)
(display (map string-length contents))
(newline)
