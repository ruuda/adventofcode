#lang racket

(define ip (open-input-file "input.txt" #:mode 'text))
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

(define (lookup board x y)
  (string-ref (list-ref board y) x))

(define (put-at x y v board)
  (string-set! (list-ref board y) x v))

(define (next-pos x y dir)
  (cond
    [(equal? dir #\^) (values x (- y 1))]
    [(equal? dir #\v) (values x (+ y 1))]
    [(equal? dir #\<) (values (- x 1) y)]
    [(equal? dir #\>) (values (+ x 1) y)]))

; Locate the position of the @ in the board.
(define (start-pos board)
  (for/or ([line (in-list board)] [y (in-naturals)])
    (for/or ([char (in-string line)] [x (in-naturals)])
      (cond
        [(equal? char #\@) (cons x y)]
        [else #f]))))

; Compute the GPS value of the board.
(define (gps-sum board)
  (for/sum ([line (in-list board)] [y (in-naturals)])
    (for/sum ([char (in-string line)] [x (in-naturals)])
      (cond
        [(equal? char #\O) (+ (* 100 y) x)]
        [else 0]))))

; Move the thing at position (x, y) in the direction `dir` if possible; returns
; whether position (x, y) is now empty, and mutates the board.
(define (move board x y dir)
  (define-values (nx ny) (next-pos x y dir))
  (let
    ([at-pos (lookup board x y)]
     [at-npos (lookup board nx ny)])
    (cond
      ; Walls can't move, and can't be moved into.
      [(equal? at-pos #\#) #f]
      [(equal? at-npos #\#) #f]
      ; Empty spaces have no need to move.
      [(equal? at-pos #\.) #t]
      ; If it's not # or . then it must be a box (O) or player (@).
      ; If the space is empty they can definitely move.
      [(equal? at-npos #\.)
       (put-at x y #\. board)
       (put-at nx ny at-pos board)
       #t]
      ; If the space was not empty, then we can try to make space there.
      [(move board nx ny dir)
       (put-at x y #\. board)
       (put-at nx ny at-pos board)
       #t]
      [else #f])))

(for ([dir (in-string moves)])
  (letrec ([xy (start-pos territory)] [x (car xy)] [y (cdr xy)])
    (move territory x y dir)
    (print territory)
    (display "\n")
    #t))

(print (gps-sum territory))
(display "\n")
