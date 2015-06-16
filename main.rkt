#lang racket/base
(require (for-syntax racket/base))
(require compatibility/defmacro)
(require racket/fixnum)
(require racket/flonum)
(provide (all-defined-out)
         #%module-begin
         
         #%datum
         #%app
         require
         
         define
         lambda
         
         module*
         
         #%top-interaction
         
         + - * /)

(define :+ fx+)
(define :* fx*)