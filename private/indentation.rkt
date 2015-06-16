#lang racket/base
(require racket/class
         racket/gui/base
         framework
         racket/sequence)
(provide determine-spaces)

(define (determine-spaces txt posi)
  (define strm (open-input-string (send txt get-text)))
  (define lines (sequence->list (in-lines strm)))
  (let loop ([rem lines]
             [idt 0]
             [cnt 0])
    (cond
      [(null? rem) (* idt 4)]
      [(> cnt posi) (* idt 4)]
      [else
       (define addamt (for/sum ([i (in-string (car rem))]
                                #:when (equal? i #\{)) 1))
       (define subamt (for/sum ([i (in-string (car rem))]
                                #:when (equal? i #\})) 1))
       (loop (cdr rem)
             (+ idt (- addamt subamt))
             (+ cnt (string-length (car rem))))])))