#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)

(define-tokens value-tokens
  (NUM ID STR))
(define-empty-tokens syntax-tokens
  (EOF
   LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
   SEMI COMMA DOT COLON
   + - * / .fl+. .fl-. .fl*. .fl/. ^ =
   === < != !== == <= >= >
   HASH
   LET SET FUN WHILE IF ELSE BREAK MARK NAMESPACE IMPORT
   ABORT RAISE DEFER RECOVER GUARD STRUCT YARN OVER TO
   ))

(define (desynid q)
  (string-append "mir-" (substring q 0 (sub1 (string-length q)))))

(define (string-parse x)
  (set! x (substring x 1 (sub1 (string-length x))))
  (string-replace x "\\n" "\n"))

(define (read-src-file x)
  (with-output-to-string
      (lambda ()
        (with-input-from-file x
          (lambda ()
            (for ([x (in-lines)])
              (match (string-trim x)
                ["" (void)]
                [(regexp #rx"^//") (void)]
                [(or (regexp #rx"{$")
                     (regexp #rx"\\[$")
                     (regexp #rx"\\($")
                     (regexp #rx";$")) (printf "~a\n" x)]
                [else (printf "~a;\n" x)])))))))

(define mir-lex
  (lexer
   ((eof) 'EOF)
   (#\# 'HASH)
   ((:: #\" (:* (:~ #\")) #\") (token-STR (string-parse lexeme)))
   
   ((:or #\tab
         #\space
         #\newline) (mir-lex input-port))
   
   ((:or "fun"
         "let"
         "while"
         "if"
         "else"
         "break"
         "mark"
         "namespace"
         "import"
         "defer"
         "set"
         "abort"
         "guard"
         "struct"
         "recover"
         "yarn"
         "over"
         "to"
         
         "="
         "==" "===" "!=" "!=="
         "<" "<=" ">" ">="
         "+" "-" "*" "/") (string->symbol (string-upcase lexeme)))
   
   ((:- (:: (:or (:/ #\a #\z)
                 (:/ #\A #\Z)
                 #\_)
        (:*
         (:or (:/ #\a #\z)
              (:/ #\A #\Z)
              #\_
              (:/ #\0 #\9)))
        (:or ""
             "::")
        (:*
         (:or (:/ #\a #\z)
              (:/ #\A #\Z)
              #\_
              (:/ #\0 #\9))))) (token-ID (string->symbol lexeme)))
   ((:: (:+ (:or (:/ #\0 #\9)))
        (:or ""
             (:: "." (:+ (:/ #\0 #\9))))) (token-NUM (string->number lexeme)))
   
   (";" 'SEMI)
   ("," 'COMMA)
   ("." 'DOT)
   (":" 'COLON)
   
   ("{" 'LBRACE)
   ("}" 'RBRACE)
   ("(" 'LPAREN)
   (")" 'RPAREN)
   ("[" 'LBRACK)
   ("]" 'RBRACK)
   ))

(define mangle-symbol
  (let ([x (gensym 'mangled)])
    (lambda (sym)
      (string->symbol
       (string-append "__" (symbol->string x) "__"
                      (symbol->string sym))))))

(define mir-parse
  (cfg-parser
   
   (start <program>)
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (error (lambda (a b c)
            (error (format "Parse error: ~v ~v ~v" a b c))))
   
   (grammar
    ;; Program header things
    (<program> ((<module-declaration> 
                 <import-declarations>
                 <semi-list>) `(_program ,$1 ,$2 (_body ,@$3))))
    (<module-declaration> ((NAMESPACE STR SEMI) $2))
    (<import-declarations> ((IMPORT STR SEMI <import-declarations>) (cons $2 $4))
                           ((SEMI) empty)
                           (() empty))
    ;; Semicolon list of expressions or declarations
    (<semi-list> ((<expr-or-decl> SEMI <semi-list>) (cons $1 $3))
                 ((<expr-or-decl>) (list $1))
                 ((SEMI) empty)
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
    ;; Expression
    (<other-expression> ((LBRACE <semi-list> RBRACE) `(let() ,@$2))
                        ((ID) $1)
                        ((NUM) $1)
                        ((STR) $1)
                        ((FUN LPAREN <comma-list> RPAREN <expression>) 
                         `(lambda ,$3 ,$5))
                        
                        ((STRUCT LPAREN <comma-list> RPAREN)
                         `(_struct ,@$3))
                        
                        ((LPAREN <expression> RPAREN) $2)
                        
                        
                        
                        ((IF LPAREN <expression> RPAREN <expression>) `(when ,$3 ,$5))
                        ((IF LPAREN <expression> RPAREN
                             <expression> ELSE <expression>) `(if ,$3 ,$5 ,$7))
                        
                        
                        
                        ((MARK ID COLON <expr-or-decl>) `(let/cc ,(mangle-symbol $2)
                                                         ,$4))
                        ((GUARD <other-expression>) `(_guard ,$2))
                        ((YARN <other-expression>) `(_yarn ,$2))
                        ((<funcall-prec>) $1)
                        )
    (<funcall-prec> ((<funcall-prec> LPAREN <comma-list> RPAREN)
                     `(_funcall ,$1 ,@$3))
                    ((<funcall-prec> LBRACK <expression> RBRACK)
                     `(_index ,$1 ,$3))
                    
                    ((<funcall-prec> DOT ID)
                     `(_member ,$1 ,$3)))
    (<expression> ((<infix-math>) $1))
    ;; Declaration
    (<declaration> ((LET ID = <expression>) `(_let ,$2 ,$4))
                   ((SET ID = <expression>) `(_set ,$2 ,$4))
                   
                   ((FUN ID LPAREN <comma-list> RPAREN <expression>) 
                         `(_let ,$2 (lambda ,$4 ,$6)))
                   ((STRUCT ID LPAREN <comma-list> RPAREN)
                         `(_let ,$2 (_struct ,@$4)))
                   
                   ((DEFER <expression>) `(_defer ,$2))
                   ((RECOVER LPAREN <expression> RPAREN <expression>)
                    `(_recover ,(list $3) ,$5))
                   ((ABORT <expression>) `(_abort ,$2))
                   ((BREAK ID <expression>) `(_funcall ,(mangle-symbol $2)
                                                       ,$3))
                   ((WHILE LPAREN <expression> RPAREN <expression>)
                    `(_while ,$3 ,$5))
                   
                   ((OVER LPAREN ID TO <expression> RPAREN <expression>)
                    `(_count_to ,$3 ,$5 ,$7))
                   
                   )
    ;; Infix math
    (<infix-math> ((<equality-prec>) $1))
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
                       ((<plus-prec>) $1))
    (<plus-prec> ((<plus-prec> + <times-prec>) `(+ ,$1 ,$3))
                 ((<plus-prec> - <times-prec>) `(- ,$1 ,$3))
                 ((<times-prec>) $1))
    (<times-prec> ((<times-prec> * <math-factor>) `(* ,$1 ,$3))
                  ((<times-prec> / <math-factor>) `(/ ,$1 ,$3))
                  ((<math-factor>) $1))
    (<math-factor> ((<other-expression>) $1))
    )))

(define (string->ast x)
  (define chugger (open-input-string x))
  (define q (mir-parse (lambda () (mir-lex chugger))))
  q)

(define (map-spare-last fun lst)
  (define slst (take lst (sub1 (length lst))))
  (append (map fun slst) (drop lst (sub1 (length lst)))))

(provide (all-defined-out))

(time
 (string->ast
 "namespace \"horrible\";
yarn haha()
"))