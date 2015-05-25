#lang racket/base
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)
(require parser-tools/yacc)
(require match-string
         racket/string
         racket/port
         racket/match
         racket/list)
(require compatibility/defmacro)
(require (for-syntax racket/base
           racket/format))
(require "lexer.rkt")
(require "ast.rkt")
(require "types.rkt")

(define-macro (liftpsn comb a b . rst)
  `(,comb (psn ,(string->symbol (format "$~a-start-pos" a)))
           (psn ,(string->symbol (format "$~a-end-pos" b)))
           . ,rst))

(define (psn x)
  (match x
    [(position a b c) (vector a b c)]))

(define mir-parse
  (parser
   
   (start <program>)
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (error (lambda (a b c d e)
            (error (format "Parse error: ~v ~v ~v ~v ~v" a b c d e))))
   
   (precs (right ++)
          (left + -)
          (left * /))
   
   (src-pos)
   
   (grammar
    ;; Program header things
    (<program> ((<import-declarations>
                 <semi-list>) (Program #f $1 $2)))
    (<import-declarations> ((IMPORT STR SEMI <import-declarations>) (cons $2 $4))
                           ((SEMI) empty)
                           (() empty))
    ;; Semicolon list of expressions or declarations
    (<semi-list> ((<expr-or-decl> SEMI <semi-list>) (cons $1 $3))
                 ((<expr-or-decl>) (list $1))
                 ((<expr-or-decl> SEMI) (list $1))
                 (() empty))
    ;; Comma list of expressions
    (<comma-list> ((<expr> COMMA <comma-list>) (cons $1 $3))
                  ((<expr>) (list $1))
                  (() empty))
    (<type-comma-list> ((<type> COMMA <type-comma-list>) (cons $1 $3))
                  ((<type>) (list $1))
                  (() empty))
    ;; Comma list of ids
    (<id-comma-list> ((ID COMMA <comma-list>) (cons $1 $3))
                     ((ID) (list $1))
                     (() empty))
    ;; Expression or declaration
    (<expr-or-decl> ((<expr>) $1)
                    ((<declaration>) $1))
    
    ;; declarations
    (<declaration> ((ID = <expr>)
                    (Def (psn $1-start-pos)
                         (psn $3-end-pos)
                         (liftpsn TAuto 1 1)
                         (Identifier (psn $1-start-pos)
                                     (psn $1-end-pos)
                                     $1)
                         $3))
                   ((ID <type> = <expr>)
                    (liftpsn Def
                             1 4
                             $2
                             (liftpsn Identifier 1 1 $1)
                             $4)))
    
    ;; most expressions
    (<expr> ((ID) (Identifier (psn $1-start-pos)
                              (psn $1-end-pos) $1))
            ((<literal>) $1)
            ((<expr> + <expr>) (Binexp (psn $1-start-pos)
                                       (psn $3-end-pos)
                                       '+
                                       $1
                                       $3))
            ((<expr> - <expr>) (Binexp (psn $1-start-pos)
                                       (psn $3-end-pos)
                                       '-
                                       $1
                                       $3))
            ((<expr> * <expr>) (Binexp (psn $1-start-pos)
                                       (psn $3-end-pos)
                                       '*
                                       $1
                                       $3))
            ((<expr> / <expr>) (Binexp (psn $1-start-pos)
                                       (psn $3-end-pos)
                                       '/
                                       $1
                                       $3))
            
            ((<expr> ++ <expr>) (liftpsn Binexp 1 3
                                        '++ $1 $3))
            )
    
    ;; literals
    (<literal> ((STR) (liftpsn StrLiteral 1 1 $1))
               ((INT) (liftpsn IntLiteral 1 1 $1))
               ((LBRACK <comma-list> RBRACK) (liftpsn TupLiteral 1 3 $2)))
    
    ;; types
    (<type> ((ID) (liftpsn TUnit 1 1 $1))
            ((ID < <type-comma-list> >) (liftpsn TParam 1 4
                                                 $1
                                                 $3)))
    
    
    )))

(define (string->ast x)
  (define chugger (pre-parse (open-input-string x)))
  (define q (mir-parse (lambda () (mir-lex chugger))))
  q)

(define (map-spare-last fun lst)
  (define slst (take lst (sub1 (length lst))))
  (append (map fun slst) (drop lst (sub1 (length lst)))))

(define (pre-parse port)
  (define str
    (with-output-to-string
        (lambda ()
          (for ([i (in-lines port)])
            (match (string-trim i)
              ["" (void)]
              [(string-append (? (λ(x) (equal? "" (string-trim x)))) "//" rst) (void)]
              [(string-append (? (λ(x) (not (equal? "" (string-trim x)))) a) "//" rst)
               (displayln (semicolify a))]
              [i (displayln (semicolify i))])))))
  (open-input-string str))

(define (semicolify ln)
  (match ln
    ["" ""]
    [(string-append _ ";") ln]
    [(string-append _ "{") ln]
    [(string-append _ ",") ln]
    [(string-append _ "(") ln]
    [_ (string-append ln ";")]))

(string->ast "x = [1, [2, 3, 4], 3]
y = [2, 3, 4]
x ++ y")