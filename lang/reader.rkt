#lang s-exp syntax/module-reader
typed/racket/base
#:read my-read
#:read-syntax my-read-syntax
#:whole-body-readers? #t

(require "../parser/main.rkt"
         "../compiler.rkt"
         racket/string
         racket/port)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (cons (datum->syntax #f
                       '(require mir)
                       #f)
        (parameterize ([SRC src])
          (finish-compile
           (compile-ast
            (string->ast
             (with-output-to-string
              (lambda()
                (copy-port ip
                           (current-output-port))))))))))