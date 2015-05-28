#lang racket/base
(require "parser/ast.rkt")
(require "parser/types.rkt"
         racket/string
         racket/list
         racket/match
         racket/pretty)
(provide (all-defined-out))

(define (compile-ast ast)
  (cond
    [(struct? ast)
     (define lol (vector->list (struct->vector ast)))
     (define name (string->symbol
                   (string-replace (symbol->string (car lol))
                                   "struct:" "")))
     (cons name (map compile-ast (rest lol)))]
    [(list? ast)
     (map compile-ast ast)]
    [else ast]))

(define SRC (make-parameter #f))

(define (finish-compile ast)
  (match ast
    [`(Program ,_ ,_ ,body)
     (map finish-compile body)]
    [(cons (cons a b) c) (datum->syntax #f
                                        (map finish-compile ast)
                                        #f)]
    [`(,NdName ,start-loc ,end-loc . ,rst)
     (pretty-print ast)
     (define args (map finish-compile rst))
     (datum->syntax #f
                    (cons NdName args)
                    (vector (SRC)
                            (vector-ref start-loc 1)
                            (vector-ref start-loc 2)
                            (vector-ref start-loc 0)
                            (- (vector-ref end-loc 0)
                               (vector-ref start-loc 0))))]
    [x (datum->syntax #f
                      x
                      #f)]))

(module+ test
  (finish-compile
   (compile-ast
    (Program
     #f
     '()
     (list
      (Def
       '#(1 1 0)
       '#(23 1 22)
       (TAuto '#(1 1 0) '#(5 1 4))
       (Identifier '#(1 1 0) '#(5 1 4) 'xaxa)
       (LstLiteral
        '#(8 1 7)
        '#(23 1 22)
        (list
         (IntLiteral '#(9 1 8) '#(10 1 9) 1)
         (IntLiteral '#(12 1 11) '#(13 1 12) 2)
         (IntLiteral '#(15 1 14) '#(16 1 15) 3)
         (IntLiteral '#(18 1 17) '#(19 1 18) 4)
         (IntLiteral '#(21 1 20) '#(22 1 21) 5)))))))))