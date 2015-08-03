#lang racket/base
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)
(require parser-tools/yacc)
(require match-string
         racket/string
         racket/port
         racket/match
         racket/list
         racket/pretty)
(require compatibility/defmacro)
(require (for-syntax racket/base
           racket/format))
(require "lexer.rkt")
(provide string->ast
         mir-parse
         pre-parse
         FILENAME)

(define FILENAME (make-parameter #f))

(define-macro (liftpsn comb a b . rst)
  `(,comb (psn ,(string->symbol (format "$~a-start-pos" a)))
          (psn ,(string->symbol (format "$~a-end-pos" b)))
          . ,rst))

(define-macro (pos-lift a b expr)
  `(let ([poz-a ,(string->symbol (format "$~a-start-pos" a))]
            [poz-b ,(string->symbol (format "$~a-end-pos" b))])
     (datum->syntax
      #f
      ,expr
      (vector
       (FILENAME)
       (position-line poz-a)
       (position-col poz-b)
       (position-offset poz-a)
       (- (position-offset poz-b)
          (position-offset poz-a))
       ))))

(define (psn x)
  (match x
    [(position a b c) (vector a b c)]))

(define mir-parse
  (parser
   
   (start <program>)
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (error (lambda (a b c poz-a poz-b)
            (raise-syntax-error
             'mir-parse
             (if c (format "Unexpected ~a (~a)" b c)
                 (format "Unexpected ~a" b))
             (datum->syntax
              #f
              (if (FILENAME) (FILENAME) "")
              (vector
               (FILENAME)
               (position-line poz-a)
               (position-col poz-b)
               (position-offset poz-a)
               (- (position-offset poz-b)
                  (position-offset poz-a)))))))
   
   (precs (nonassoc < > <= >= ==)
          (right ++)
          (left + - :+ :-)
          (left * /))
   
   (src-pos)
   ;(yacc-output "/scratch/loool.txt")
   ;(debug "/scratch/hahaha.txt")
   
   (grammar
    ;; Program header things
    (<program> ((<import-declarations>
                 <semi-list>) (append $1 $2)))
    (<import-declarations> ((USING STR SEMI <import-declarations>) (cons `(using ,$2) $4))
                           ((SEMI) empty)
                           (() empty))
    ;; Semicolon list of expressions or declarations
    (<semi-list> ((<expr-or-decl> SEMI <semi-list>) (cons $1 $3))
                 ((<expr-or-decl>) (list $1))
                 (() empty))
    ;; Comma list of expressions
    (<comma-list> ((<blk> COMMA <comma-list>) (cons $1 $3))
                  ((<blk>) (list $1))
                  (() empty))
    (<comma-last-list> ((<blk> COMMA <comma-list>) (cons $1 $3))
                       ((<blk> COMMA) (list $1))
                       (() empty))
    ;; Comma list of ids
    (<id-comma-list> ((<id> COMMA <comma-list>) (cons $1 $3))
                     ((<id>) (list $1))
                     (() empty))
    ;; Expression or declaration
    (<expr-or-decl> ((<blk>) $1)
                    ((<declaration>) $1))
    
    ;; declarations
    (<declaration> ((DEF <definition>) $2))
    (<definition> ((<id> = <blk>)
                   (pos-lift 1 3 `(define ,$1 ,$3)))
                  ((<id> LPAREN <id-comma-list> RPAREN = <blk>)
                   (pos-lift 1 6 `(begin
                                    (define (,$1 . ,$3) ,$6))))
                  )
    
    (<id> ((ID) (pos-lift 1 1 $1)))
    
    ;; block-precedence expressions
    (<blk> ((IF <blk> THEN <blk> ELSE <blk>) (pos-lift 1 6
                                                       `(if ,$2 ,$4 ,$6)))
           
           ((LPAREN <id-comma-list> RPAREN -> <blk>) (pos-lift 1 5
                                                               `(lambda ,$2 ,$5)))
           ((<expr>) $1))
    
    ;; most expressions
    (<expr> ((<expr> + <expr>) (pos-lift 1 3 `(+ ,$1 ,$3)))
            ((<expr> :+ <expr>) (pos-lift 1 3 `(:+ ,$1 ,$3)))
            ((<expr> - <expr>) (pos-lift 1 3 `(- ,$1 ,$3)))
            ((<expr> :- <expr>) (pos-lift 1 3 `(:- ,$1 ,$3)))
            ((<expr> * <expr>) (pos-lift 1 3 `(* ,$1 ,$3)))
            ((<expr> / <expr>) (pos-lift 1 3 `(/ ,$1 ,$3)))
            
            ((<expr> == <expr>) (pos-lift 1 3 `(eqv? ,$1 ,$3)))
            ((<expr> < <expr>) (pos-lift 1 3 `(< ,$1 ,$3)))
            
            ((<expr> ++ <expr>) (pos-lift 1 3 `(append ,$1 ,$3)))
            ((<tight>) $1)
            )
    
    ;; tight-binding expressions
    (<tight> ((<tight> LPAREN <comma-list> RPAREN) (pos-lift 1 4
                                                             `(,$1 . ,$3)))
             ((LPAREN <blk> RPAREN) $2)
             ((LBRACE <semi-list> RBRACE) (pos-lift 1 3
                                                    `(let() . ,$2)))
             ((__RSPLICE__ LPAREN STR RPAREN) (pos-lift 1 4
                                                        (read (open-input-string $3))))
             ((<literal>) $1))
    
    ;; literals
    (<literal> ((STR) (pos-lift 1 1 $1))
               ((INT) (pos-lift 1 1 $1))
               ((LPAREN <comma-last-list> RPAREN) (pos-lift 1 3 `(__vector__ ,@$2)))
               ((LBRACK <comma-list> RBRACK) (pos-lift 1 3 `(__list__ ,@$2)))
               ((<id>) $1))
    
    
    )))


(define (string->ast x)
  (define chugger (pre-parse (open-input-string x)))
  (define q (mir-parse (lambda () (mir-lex chugger))))
  q)

(define (map-spare-last fun lst)
  (define slst (take lst (sub1 (length lst))))
  (append (map fun slst) (drop lst (sub1 (length lst)))))

(define (pre-parse port)
  (define-values (in out) (make-pipe))
  (for ([i (in-lines port)])
    (displayln (semicolify i) out))
  (close-output-port out)
  in)

(define (semicolify ln)
  (match ln
    ["" ""]
    [(string-append _ ";") ln]
    [(string-append _ "{") ln]
    [(string-append _ ",") ln]
    [(string-append _ "(") ln]
    [_ (string-append ln ";")]))

;(string->ast "f(x Integer) Integer = x;")
