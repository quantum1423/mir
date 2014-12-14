#lang racket
(require syntax/strip-context)
(require "../parser.rkt")
(require racket/provide)

(provide (rename-out [mir-read read]
                     [mir-read-syntax read-syntax])
         get-info)

(define (mir-read (in (current-input-port)))
  (ast-prepare (string->ast (port->string (pre-parse in)))))

(define (mir-read-syntax src in)
  (strip-context
   (datum->syntax #f (mir-read in))))

(define (ast-prepare ast)
  (define haha
    (match ast
    [`(_program ,modname
                ,imports
                ,body)
     `(module lolo typed/racket/base
        (require mirstdlib)
        (_export_things)
        (require racket/provide)
        (require racket/require)
        (require (for-syntax racket))
        ,@(for/list ([imp imports])
            `(_import ,imp))
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