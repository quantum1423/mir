#lang s-exp syntax/module-reader
typed/racket/base
#:read my-read
#:read-syntax my-read-syntax
#:whole-body-readers? #t

(require "../parser/main.rkt"
         "../parser/lexer.rkt"
         "../compiler.rkt"
         racket/string
         racket/port)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (define lol ip)
  (parameterize ([FILENAME src])
    (cons (datum->syntax #f
                         '(require mir)
                         #f)
          (mir-parse
           (lambda()
             (mir-lex
              lol))))))