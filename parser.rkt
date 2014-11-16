#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(define-tokens value-tokens
  (NUM ID SYNID))
(define-empty-tokens syntax-tokens
  (EOF
   LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
   SEMI COMMA DOT COLON
   + - * / ^ :=
   == <
   ))

(define (desynid q)
  (string-append "mir-" (substring q 0 (sub1 (string-length q)))))

(define mir-lex
  (lexer
   ((eof) 'EOF)
   ((:or #\tab
         #\space
         #\newline) (mir-lex input-port))
   ((:: (:or (:/ #\a #\z)
             (:/ #\A #\Z)
             #\_)
        (:*
         (:or (:/ #\a #\z)
              (:/ #\A #\Z)
              #\_
              (:/ #\0 #\9)))) (token-ID (string->symbol lexeme)))
   ((:: (:or (:/ #\a #\z)
             (:/ #\A #\Z)
             #\_)
        (:*
         (:or (:/ #\a #\z)
              (:/ #\A #\Z)
              #\_
              (:/ #\0 #\9)))
        #\:) (token-SYNID (string->symbol (desynid lexeme))))
   ((:: (:+ (:or (:/ #\0 #\9)))) (token-NUM (string->number lexeme)))
   
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)
   ("^" '^)
   ("=" ':=)
   
   ("==" '==)
   ("<" '<)
   
   (";" 'SEMI)
   ("," 'COMMA)
   
   ("{" 'LBRACE)
   ("}" 'RBRACE)
   ("(" 'LPAREN)
   (")" 'RPAREN)
   ("[" 'LBRACK)
   ("]" 'RBRACK)
   ))

(define mir-parse
  (parser
   
   (start start)
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (error (lambda (a b c)
            (error (format "Parse error: ~v ~v ~v" a b c))))
   
   (precs (right :=)
          (left == <)
          (left SYNID)
          (left - +)
          (left * /))
   
   (grammar
    (start ((semilst) `(let() ,@$1)))
    (expr ((ID := expr) `(define ,$1 ,$3))
          ((ID) $1)
          ((LBRACE semilst RBRACE) `(let () ,@$2))
          ((NUM) $1)
          ((expr LPAREN commalst RPAREN) `(,$1 ,@$3))
          ((LBRACK commalst RBRACK) $2)
          
          ((LPAREN expr RPAREN) $2)
          ((SYNID synob) `(,$1 ,@$2))
          
          ((expr + expr) `(+ ,$1 ,$3))
          ((expr - expr) `(- ,$1 ,$3))
          ((expr * expr) `(* ,$1 ,$3))
          ((expr / expr) `(/ ,$1 ,$3))
          ((expr == expr) `(equal? ,$1 ,$3))
          ((expr < expr) `(< ,$1 ,$3))
          )
    
    (semilst ((expr SEMI semilst) (cons $1 $3))
             (() empty))
    
    (commalst ((expr COMMA commalst) (cons $1 $3))
              ((expr) (list $1))
              (() empty))
    
    (synob ((expr synob) (cons $1 $2))
           ((expr) (list $1)))
    )))

(provide (all-defined-out))