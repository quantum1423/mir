#lang racket
(require syntax/strip-context)
(require "../parser.rkt")
(require racket/provide)
(provide (rename-out [mir-read read]
                     [mir-read-syntax read-syntax])
         get-info)
(define (mir-read (in (current-input-port)))
  (ast-prepare (string->ast (pre-parse in))))
(define (mir-read-syntax src in)
  (strip-context
   (datum->syntax #f (mir-read in))))
(define (ast-prepare ast)
  (define haha
    (match ast
      [`(@program ,modname
                  ,imports
                  ,body)
       `(module lolo racket/base
          (require mirstdlib)
          (provide (all-defined-out))
          (require racket/provide)
          (require racket/require)
          (require racket/port)
          (require (for-syntax racket/base
                               racket/string))
          ,@(for/list ([imp imports])
              `(@import ,imp))
          ,@(cdr body)
          )]))
  haha)
(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/default-lexer
                        'default-lexer)]
      [else default])))