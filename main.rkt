#lang racket/base
(require (for-syntax racket/base))
(require compatibility/defmacro)
(require mir/core/main)
(provide (all-defined-out)
         (all-from-out mir/core/main))

;; Calling functions
(define-macro (@funcall a . b)
  (cons a b))