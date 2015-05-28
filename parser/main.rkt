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
(provide string->ast)

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
                 (() empty))
    ;; Comma list of expressions
    (<comma-list> ((<blk> COMMA <comma-list>) (cons $1 $3))
                  ((<blk>) (list $1))
                  (() empty))
    (<comma-last-list> ((<blk> COMMA <comma-list>) (cons $1 $3))
                       ((<blk> COMMA) (list $1))
                       (() empty))
    (<type-comma-list> ((<type> COMMA <type-comma-list>) (cons $1 $3))
                       ((<type>) (list $1))
                       (() empty))
    ;; Comma list of ids
    (<id-comma-list> ((ID COMMA <comma-list>) (cons $1 $3))
                     ((ID) (list $1))
                     (() empty))
    ;; Argument list
    (<arg-list> ((<id> <type> COMMA <arg-list>) (cons (cons $1 $2) $4))
                ((<id> <type>) (list (cons $1 $2)))
                (() empty))
    ;; Expression or declaration
    (<expr-or-decl> ((<blk>) $1)
                    ((<declaration>) $1))
    
    ;; declarations
    (<declaration> ((ID = <blk>)
                    (Def (psn $1-start-pos)
                         (psn $3-end-pos)
                         (liftpsn TAuto 1 1)
                         (Identifier (psn $1-start-pos)
                                     (psn $1-end-pos)
                                     $1)
                         $3))
                   ((ID <type> = <blk>)
                    (liftpsn Def
                             1 4
                             $2
                             (liftpsn Identifier 1 1 $1)
                             $4))
                   ((<id> LPAREN <arg-list> RPAREN <type> = <blk>)
                    (liftpsn Def
                             1 7
                             $5
                             $1
                             (liftpsn FunLiteral
                                      7 7
                                      $3
                                      $7))))
    
    (<id> ((ID) (Identifier (psn $1-start-pos)
                            (psn $1-end-pos) $1)))
    
    ;; block-precedence expressions
    (<blk> ((IF <blk> THEN <blk> ELSE <blk>) (liftpsn If 1 6
                                                      $2
                                                      $4
                                                      $6))
           ((FOR <blk> DO <blk>) (error "WHILE not supported"))
                          
           ((FUN LPAREN <arg-list> RPAREN <expr>) (liftpsn FunLiteral 1 5
                                                           $3
                                                           $5))
           ((<expr>) $1))
    
    ;; most expressions
    (<expr> ((<expr> + <expr>) (Binexp (psn $1-start-pos)
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
            ((<tight>) $1)
            )
    
    ;; tight-binding expressions
    (<tight> ((<tight> LPAREN <comma-list> RPAREN) (liftpsn Funcall 1 4
                                                            $1 $3))
             ((LPAREN <blk> RPAREN) $2)
             ((LBRACE <semi-list> RBRACE) (liftpsn Block 1 3 $2))
             ((<literal>) $1))
    
    ;; literals
    (<literal> ((STR) (liftpsn StrLiteral 1 1 $1))
               ((INT) (liftpsn IntLiteral 1 1 $1))
               ((LPAREN <comma-last-list> RPAREN) (liftpsn TupLiteral 1 3 $2))
               ((LBRACK <comma-list> RBRACK) (liftpsn LstLiteral 1 3 $2))
               ((<id>) $1))
    
    ;; types
    (<type> ((ID) (liftpsn TUnit 1 1 $1))
            ((ID < <type-comma-list> >) (liftpsn TParam 1 4
                                                 $1
                                                 $3))
            ((FUN LPAREN <type-comma-list> RPAREN <type>) (liftpsn TFunction 1 5
                                                                   $3
                                                                   $5)))
    
    
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
  ;(displayln str)
  (open-input-string str))

(define (semicolify ln)
  (match ln
    ["" ""]
    [(string-append _ ";") ln]
    [(string-append _ "{") ln]
    [(string-append _ ",") ln]
    [(string-append _ "(") ln]
    [_ (string-append ln ";")]))

#;(string->ast
 "xaxa = [1, 2, 3, 4, 5]")