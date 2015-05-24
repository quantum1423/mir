#lang racket/base
(provide (all-defined-out))
(require (prefix-in ra- pfds/ralist/skew)
         racket/format
         racket/string
         racket/function)

(define @list list)
(define %%list list)
(define %%list? list?)
(define @curry curry)

(define (%%toString lol)
  (cond
    [(string? lol) lol]
    [(%%list? lol)
     (string-append
      "["
      (string-append*
       (for/list ([i (in-list lol)]
                  [ctr (length lol)])
         (string-append
          (%%toString i)
          (if (= ctr (sub1 (length lol))) ""
              ", "))))
      
      "]")]
    [else (~a lol)]))

(define (%%println x)
  (displayln (%%toString x)))