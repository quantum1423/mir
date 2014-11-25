#lang racket

;; Static sanity check thing.

(define (static-check expr)
  ; First pass: top-level defs
  (define topdefs
    (let loop ([s (set)]
               [e expr])
      (match e
        [`((define ,a ,_) . ,rst) (loop (set-add s a)
                                             rst)]
        [`(,a . ,b) (loop s b)]
        ['() s])))
  ; 

(static-check
 `((define xaxa 100)))