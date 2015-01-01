#lang racket/base

(require syntax/strip-context)
(require "../parser.rkt")
(require racket/provide
         racket/port
         racket/match)

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
      [`(@program ,modname
                  ,imports
                  ,body)
       `(module compiled-racket racket/base
          (require mirstdlib)
          (require racket/unsafe/ops)
          (require racket/require)
          (require (for-syntax racket/base))
          (require (for-syntax racket/string))
          (provide (all-defined-out))
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

(define (emit-import-decl imp)
  (match imp
    [(regexp #rx"\\.mir$") `(require ,imp)]
    [(regexp #rx"\\.rkt$") `(require ,imp)]
    [else `(require (filtered-in
                     (lambda (name)
                       (and
                        (regexp-match #rx"^\\*\\*MIR-ID\\*\\*[A-Z]" name)
                        (string-append (first (reverse (string-split imp "/")))
                                       "::"
                                       name)))
                     (lib ,(string-append imp "/main.mir"))))]))