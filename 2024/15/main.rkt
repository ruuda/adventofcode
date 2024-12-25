#lang racket

(define ip (open-input-file "example.txt" #:mode 'text))

(let loop ()
  (display "Reading one line ... ")
  (let ([line (read-line ip 'any)])
    (cond
      [(eq? line eof)
       (display "Got EOF")]
      [else
       (display "Got line: ")
       (display line)
       (display "\n")
       (loop)])))

(display "\nDone\n")
