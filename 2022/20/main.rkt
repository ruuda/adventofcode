#!/usr/bin/env racket
#lang racket

(define file (open-input-file "example.txt"))
(define (readfile)
  (let
    ((line (read-line file)))
    (cond
      ((eof-object? line) '())
      (else (cons line (readfile))))))

(define contents (readfile))
(define numbers (map string->number contents))
(display numbers)
