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
(define (zip l1 l2) (map list l1 l2))
(define workset
  (list->vector (zip (range (length numbers)) numbers)))

(define (rotate-at vec i)
  (letrec
    ((pfx (vector-take vec i))
     (sfx (vector-drop vec (+ i 1)))
     (pn     (vector-length pfx))
     (sn     (vector-length sfx))
     (elem   (vector-ref vec i))
     (shift  (car (cdr elem))))
    (cond
      ((and (>= shift 0) (<= shift pn))
        (let-values
          (((pa pb) (vector-split-at sfx shift)))
          (vector-append pfx pa #(elem) pb)))
      (else
        (vector-append pfx #(elem) sfx)))))

(display workset)
(newline)
(display (rotate-at workset 0))
