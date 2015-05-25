#lang typed/racket/base
(provide (all-defined-out))
(require racket/unsafe/ops)
(define-type Position (Vector Integer
                              (Option Integer)
                              (Option Integer)))

(define-type Type (U TAuto TUnit TParam))

(struct TAuto
  ([start-pos : Position]
   [end-pos : Position])
  #:transparent)

(struct TUnit
  ([start-pos : Position]
   [end-pos : Position]
   [name : Symbol])
  #:transparent)

(struct TParam
  ([start-pos : Position]
   [end-pos : Position]
   [name : Symbol]
   [args : (Listof Type)])
  #:transparent)