#lang racket

(define ip (open-input-file "example1.txt" #:mode 'text))
(define-values
  (territory-list moves-list)
  (let loop (
    [mode 'territory]
    [territory '()]
    [moves '()])
    (let ([line (read-line ip 'any)])
      (cond
        [(eq? line eof)
         (values territory moves)]
        [(equal? line "")
         (loop 'moves territory moves)]
        [(equal? mode 'territory)
         (loop 'territory (cons line territory) moves)]
        [else
         (loop 'moves territory (cons line moves))]))))

(define territory (reverse territory-list))
(define moves (string-join (reverse moves-list) ""))



(print moves)
