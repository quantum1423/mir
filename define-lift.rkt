#lang racket
(provide lift-definitions)

(define (lift-definitions stuff)
  (define todef
    (filter
     (lambda (thing)
       (match thing
         [`(define ,a ,_) #t]
         [_ #f])) stuff))
  `(let()
     ,@(for/list ([i (map second todef)])
         `[define ,i #f])
     ,@(for/list ([line stuff])
         (match line
           [`(define ,a ,b)
            `(@redef ,a ,b)]
           [x x]))))