#lang racket
(require syntax/strip-context)

(define (ast-postproc ast)
  (match ast
    [`(let ()
        (mir-module ,name)
        . ,rst)
     `(module anything racket
        