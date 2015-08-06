#lang racket

(define (mangle x)
  (string->symbol (string-append "_User_" (symbol->string x))))

(define (new-temp)
  (symbol->string (gensym '_Temp_)))

(define (sexp->cee sexp)
  (match sexp
    [(? symbol? x) (values "" (~a (mangle x)))]
    [(? integer? x) (let ([x (format "mirIntTo(~a)" x)]) (values "" x))]
    [`(%smi+ ,x ,y) (let-values ([(x-c x-r) (sexp->cee x)]
                                 [(y-c y-r) (sexp->cee y)]
                                 [(res-name) (new-temp)])
                      (values
                       (string-append
                        x-c
                        y-c
                        (format "mirObject ~a = mirIntAdd(~a, ~a);\n" res-name
                                x-r
                                y-r))
                       res-name))]
    
    [`(%do . ,rst)
     (define res-name (new-temp))
     (values
      (string-append
       (format "mirObject ~a;\n" res-name)
       "{\n"
       (string-append*
        (for/list ([x rst])
          (match x
            [`(%let ,k ,v) (let-values ([(k-c k-r) (sexp->cee k)]
                                        [(v-c v-r) (sexp->cee v)])
                             (string-append
                              v-c
                              (format "mirObject ~a = ~a;\n" k-r v-r)))]
            [_ (let-values ([(x-c x-r) (sexp->cee x)])
                 (string-append
                  x-c
                  (format "~a = ~a;\n" res-name x-r)))])))
       "}\n")
      (~a res-name))]))

(define-values (code res)
  (sexp->cee '(%smi+ 123
                     (%do
                      (%let foo 12345)
                      (%let bar 54321)
                      (%smi+ foo (%smi+ foo bar))))))
(display code)