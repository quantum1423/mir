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
(require "lexer.rkt")

(define mir-parse
  (cfg-parser
   
   (start <program>)
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (error (lambda (a b c)
            (error (format "Parse error: ~v ~v ~v" a b c))))
   
   (grammar
    ;; Program header things
    (<program> ((<import-declarations>
                 <semi-list>) `(@program #f ,$1 (@body ,@$2)))
               ((<expr-or-decl>) $1))
    (<import-declarations> ((IMPORT STR SEMI <import-declarations>) (cons $2 $4))
                           ((SEMI) empty)
                           (() empty))
    ;; Semicolon list of expressions or declarations
    (<semi-list> ((<expr-or-decl> SEMI <semi-list>) (cons $1 $3))
                 ((<expr-or-decl>) (list $1))
                 ((<expr-or-decl> SEMI) (list $1))
                 (() empty))
    ;; Comma list of expressions
    (<comma-list> ((<expression> COMMA <comma-list>) (cons $1 $3))
                  ((<expression>) (list $1))
                  (() empty))
    ;; Comma list of ids
    (<id-comma-list> ((ID COMMA <comma-list>) (cons $1 $3))
                     ((ID) (list $1))
                     (() empty))
    ;; Expression or declaration
    (<expr-or-decl> ((<expression>) $1)
                    ((<declaration>) $1))
    (<expression> ((<infix-math>) $1))
    ;; Declaration
    (<declaration> ((ID <- <expression>) `(set! ,$1 ,$3))
                   ((ID = <expression>) `(define ,$1 ,$3))
                   ((ID LPAREN <id-comma-list> RPAREN = <expression>)
                    `(define ,$1 (lambda ,$3 ,$6)))
                   ((ID LPAREN <id-comma-list> COMMA ID ... RPAREN = <expression>)
                    `(define ,$1 (@lambda-vargs ,$3 ,$5 ,$9)))
                   
                   
                   ((DEFER <expression>) `(@defer ,$2))
                   ((RECOVER LPAREN <expression> RPAREN <expression>)
                    `(@recover ,(list $3) ,$5))
                   ((ERROR <expression>) `(error ,$2))
                   ((BREAK) `(@break (void)))
                   ((BREAK <expression>) `(@break ,$2))
                   )
    ;; Infix math
    (<infix-math> ((<structure-prec>) $1))
    
    ;; Lowest precedence: structures like if, while, etc
    (<structure-prec> ((WHEN <block-prec> DO <structure-prec>)
                       `(when ,$2 ,$4))
                      ((IF <block-prec> THEN <structure-prec>
                           ELSE <structure-prec>)
                       `(if ,$2 ,$4 ,$6))
                      
                      ((WHILE <block-prec> <structure-prec>)
                       `(let/ec @break (@while ,$2 ,$3)))
                      
                      ((FOR <id-comma-list> RANGE <block-prec> <structure-prec>)
                       `(let/ec @break (for ([,$2 ,$4]) ,$5)))
                      ((FOR <id-comma-list> RANGE <block-prec> WHERE <block-prec> <structure-prec>)
                       `(let/ec @break (for ([,$2 ,$4] #:when ,$6) ,$7)))
                      
                      ((FOR <id-comma-list> RANGE <block-prec> COLLECT <structure-prec>)
                       `(for/list ((,$2 ,$4)) ,$6))
                      ((FOR <id-comma-list> RANGE <block-prec> WHERE <block-prec> COLLECT <structure-prec>)
                       `(for/list ((,$2 ,$4) #:when ,$6) ,$8))
                      
                      ((FUN LPAREN <id-comma-list> RPAREN
                            <structure-prec>)
                       `(lambda ,$3 ,$5))
                      
                      ((FUN LPAREN <id-comma-list> COMMA ID ... RPAREN
                            <structure-prec>)
                       `(@lambda-vargs ,$3 ,$5 ,$8))
                      
                      ((YARN <structure-prec>) `(@yarn ,$2))
                      ((GUARD <structure-prec>) `(@guard ,$2))
                      ((UNSAFE <structure-prec>) `(@unsafe ,$2))
                      ((MARK <structure-prec>)
                       `(let/ec @break
                          ,$2))
                      
                      ((OBJECT LBRACE <semi-list> RBRACE)
                       `(@object ,@$3))
                      
                      ((OBJECT FROM <structure-prec> LBRACE <semi-list> RBRACE)
                       `(@object-inherit ,$3 ,@$5))
                      
                      ((<structure-prec> INTO <block-prec>) `(@apply ,$3 ,$1))
                      
                      ((<block-prec>) $1))
    
    ;; Second precedence: blocks {...}
    (<block-prec> ((LBRACE <semi-list> RBRACE) `(let() ,@$2))
                  ((LBRACE RBRACE) `(void))
                  ((<and-prec>) $1))
    
    (<and-prec> ((<and-prec> AND <or-prec>) `(and ,$1 ,$3))
                ((<or-prec>) $1))
    
    (<or-prec> ((<or-prec> OR <equality-prec>) `(or ,$1 ,$3))
               ((<equality-prec>) $1))
    
    (<equality-prec> ((<equality-prec> === <comparison-prec>) `(eqv? ,$1 ,$3))
                     ((<equality-prec> !== <comparison-prec>) 
                      `(not (eqv? ,$1 ,$3)))
                     ((<equality-prec> == <comparison-prec>) `(equal? ,$1 ,$3))
                     ((<equality-prec> != <comparison-prec>) `(not (equal? ,$1 ,$3)))
                     ((<comparison-prec>) $1))
    (<comparison-prec> ((<comparison-prec> < <plus-prec>) `(< ,$1 ,$3))
                       ((<comparison-prec> <= <plus-prec>) `(<= ,$1 ,$3))
                       ((<comparison-prec> > <plus-prec>) `(> ,$1 ,$3))
                       ((<comparison-prec> >= <plus-prec>) `(>= ,$1 ,$3))
                       ((<append-prec>) $1))
    (<append-prec> ((<plus-prec> ++ <append-prec>) `(@append ,$1 ,$3))
                   ((<plus-prec>) $1))
    (<plus-prec> ((<plus-prec> + <times-prec>) `(+ ,$1 ,$3))
                 ((<plus-prec> - <times-prec>) `(- ,$1 ,$3))
                 ((<times-prec>) $1))
    (<times-prec> ((<times-prec> * <unary-prec>) `(* ,$1 ,$3))
                  ((<times-prec> % <unary-prec>) `(modulo ,$1 ,$3))
                  ((<times-prec> / <unary-prec>) `(/ ,$1 ,$3))
                  ((<times-prec> \\ <unary-prec>) `(quotient ,$1 ,$3))
                  ((<unary-prec>) $1))
    
    ;; Unary operators. Shitty shim currently.
    (<unary-prec> ((<funcall-prec>) $1))
    
    ;; Funcall-like things.
    (<funcall-prec> ((<funcall-prec> LPAREN <comma-list> RPAREN)
                     `(@funcall ,$1 ,@$3))
                    
                    ((<funcall-prec> LPAREN <comma-list> COMMA ID ... RPAREN)
                     `(@funcall-with-rst ,$1 ,$3 ,$5))
                    
                    ((<funcall-prec> LPAREN <expression> ... RPAREN)
                     `(@funcall-with-rst ,$1 () ,$3))
                    
                    ((<funcall-prec> LBRACK <expression> RBRACK)
                     `(@index ,$1 ,$3))
                    
                    ((<funcall-prec> LBRACK <expression> COLON <expression> RBRACK)
                     `(@range ,$1 ,$3 ,$5))
                    
                    ((<funcall-prec> LBRACK <expression> COLON RBRACK)
                     `(@range ,$1 ,$3))
                    
                    ((<funcall-prec> LBRACK COLON <expression>  RBRACK)
                     `(@range ,$1 0 ,$4))
                    
                    ((<funcall-prec> DOT <literal-prec>)
                     `(@curry ,$3 ,$1))
                    
                    ((LPAREN <comma-list> RPAREN DOT <literal-prec>)
                     `(@multicurry ,$5 ,$2))
                    
                    ((<literal-prec>) $1))
    
    (<literal-prec> ((ID)  $1)
                    ((NUM) $1)
                    ((STR) $1)
                    ((THIS) '(current-object))
                    ((BTS) (string->bytes/utf-8 $1))
                    ((LPAREN <expression> RPAREN) $2)
                    ((LBRACK <comma-list> RBRACK) (cons '@list $2)))
    
    )))

(define (string->ast x)
  (define chugger (open-input-string x))
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
  ;(display str)
  str)

(define (semicolify ln)
  (match ln
    ["" ""]
    [(string-append _ ";") ln]
    [(string-append _ "{") ln]
    [(string-append _ ",") ln]
    [(string-append _ "(") ln]
    [_ (string-append ln ";")]))

(provide (all-defined-out))