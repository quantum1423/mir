#lang s-exp syntax/module-reader
mir
#:read my-read
#:read-syntax my-read-syntax
#:whole-body-readers? #t
#:info (lambda (key default . rst)
         (case key
           [(color-lexer)
            (dynamic-require 'syntax-color/default-lexer
                             'default-lexer)]
           [(drracket:indentation)
            (with-output-to-file "/scratch/HAHA.txt"
              #:exists 'append
              (lambda()
                (displayln "HAHAHAA")))
            (dynamic-require 'mir/private/indentation 'determine-spaces)]
           [else default]))


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
                         `(module* configure-runtime racket/base
                            (#%plain-module-begin
                             (#%require mir/parser/main mir/parser/lexer)
                             (current-read-interaction
                              (lambda (name in)
                                (define hoho (read-line in))
                                (cond 
                                  [(eof-object? hoho) hoho]
                                  [else
                                   (define lolo (open-input-string hoho))
                                   (car (mir-parse (lambda () (mir-lex lolo))))]))))))
          (cons (datum->syntax #f
                               '(require mir)
                               #f)
                (mir-parse
                 (lambda()
                   (mir-lex
                    lol)))))))