#lang typed/racket/base
(require (for-syntax racket/base))
(require compatibility/defmacro)
(require mir/core/main)
(provide (all-defined-out)
         (all-from-out mir/core/main)
         #%module-begin
         #%datum)

;; Definition
(define-macro (Def t l r)
  `(begin
     ,(if (equal? t '(TAuto))
          `(define ,(cadr l) ,r)
          `(define ,(cadr l) : ,t ,r))))

;; Infix operation
(define-macro (Binexp o l r)
  (list o l r))

;; Literals
(define-macro (IntLiteral x)
  x)
(define-macro (Identifier x)
  x)

;; Types
(define-type (TUnit x) x)