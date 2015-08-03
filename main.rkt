#lang racket/base
(require (for-syntax racket/base
           racket/string))
(require compatibility/defmacro)
(require racket/fixnum)
(require racket/flonum
         racket/require)
(provide #%module-begin
         #%top
         module*
         require
         #%app
         #%top-interaction
         #%datum
         define
         begin
         
         displayln
         using
         file
         provide

         let
         if

         prefix-in
         matching-identifiers-in
         
         all-defined-out
         (all-defined-out)
         
         + - * / < > <= >=
         )

(define-macro (__thingy__ verb str)
  (cond
    [(regexp-match #rx"^\\." str)
     (cond
       [(regexp-match #rx"/$" str) `(,verb (file ,(string-append str "main.mir")))]
       [else `(require ,str)])]
    [else (let ([last-part (car (reverse (string-split str "/")))])
            (define toreq (if (file-exists? (string-append "/home/sawatari/Documents/Programming/racket/mir/libraries/"
                                                           str
                                                           "/main.mir"))
                              (string-append "/home/sawatari/Documents/Programming/racket/mir/libraries/"
                                             str
                                             "/main.mir")
                              
                              (string-append "/home/sawatari/Documents/Programming/racket/mir/libraries/"
                                             str
                                             ".mir")))
            `(,verb (prefix-in ,(string->symbol
                                 (string-append last-part
                                               "::"))
                               (matching-identifiers-in #rx"^[A-Z]" (file ,toreq)))))]))

(define-macro (using str)
  `(__thingy__ require ,str))
(define-macro (sharing str)
  `(__thingy__ require/provide ,str))

(define-macro (require/provide lol)
  `(begin
     (require ,lol)
     (provide (all-from-out ,(if (equal? (car lol) 'file) lol (caddr lol))))))

(define __printf__ printf)
(define __list__ list)