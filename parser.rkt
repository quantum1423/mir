#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(define-tokens value-tokens
  (NUM ID SYNID STR))
(define-empty-tokens syntax-tokens
  (EOF
   LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
   SEMI COMMA DOT COLON
   + - * / ^ :=
   == <
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
   ((:: #\" (:* (:~ #\")) #\") (token-STR (string-parse lexeme)))
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
              (:/ #\0 #\9)))
        (:or ""
             "::")
        (:*
         (:or (:/ #\a #\z)
              (:/ #\A #\Z)
              #\_
              (:/ #\0 #\9)))
        
        ) (token-ID (string->symbol lexeme)))
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
          ((STR) $1)
          ((expr LPAREN commalst RPAREN) `(call ,$1 ,@$3))
          ((LBRACK commalst RBRACK) (cons 'list $2))
          
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

(define (string->ast x)
  (define chugger (open-input-string x))
  (mir-parse (lambda () (mir-lex chugger))))

(provide (all-defined-out))