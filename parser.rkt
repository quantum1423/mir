#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)
(require match-string)

(define-tokens value-tokens
  (NUM ID STR TYPE))
(define-empty-tokens syntax-tokens
  (EOF
   LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
   SEMI COMMA DOT COLON
   + - * / % .fl+. .fl-. .fl*. .fl/. ^ = :=
   === < != !== == <= >= > -> ++
   HASH
   FUN IF ELSE BREAK MARK NAMESPACE IMPORT FOR
   ABORT RAISE DEFER RECOVER GUARD STRUCT YARN TO DEF
   RETURN
   VBAR MAP IN WHERE
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
  (lexer-src-pos
   ((eof) 'EOF)
   (#\# 'HASH)
   ((:: #\" (:* (:~ #\")) #\") (token-STR (string-parse lexeme)))
   
   ((:or #\tab
         #\space
         #\newline) (position-token-token (mir-lex input-port)))
   
   ((:or "fun"
         "if"
         "else"
         "break"
         "mark"
         "namespace"
         "import"
         "defer"
         "abort"
         "guard"
         "struct"
         "recover"
         "yarn"
         "for"
         "def"
         "to"
         "return"
         "map"
         "dcast"
         "scast"
         "is"
         "in"
         "where"
         
         "="
         "==" "++" "!=" "!=="
         "<" "<=" ">" ">=" "->"
         "+" "-" "*" "/" "%") (string->symbol (string-upcase lexeme)))
   
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
   ("$[" 'LTUP)
   ("|" 'VBAR)
   ))

(define mangle-symbol
  (let ([x (gensym 'mangled)])
    (lambda (sym)
      (string->symbol
       (string-append "__" (symbol->string x) "__"
                      (symbol->string sym))))))

(define STRING "")

(define (STRING-VICINITY i j)
  (substring STRING (max 0 (- (position-offset i) 10))
             (min (+ 10 (position-offset j)) (sub1 (string-length STRING)))))

(define mir-parse
  (cfg-parser
   
   (start <program>)
   (end EOF)
   (src-pos)
   (tokens value-tokens syntax-tokens)
   (error (lambda (a b c i j)
            (printf "Parse error:\n")
            (printf "  Unexpected token ~a\n" b)
            (printf "  ~a\n" (STRING-VICINITY i j))
            (exit 42)))
   
   (grammar
    ;; Program header things
    (<program> ((<module-declaration>
                 <import-declarations>
                 <semi-list>) `(_program ,$1 ,$2 (_body ,@$3)))
               ((<import-declarations>
                 <semi-list>) `(_program ,(symbol->string
                                           (gensym '__TT__))
                                         ,$1 (_body ,@$2))))
    (<module-declaration> ((NAMESPACE ID SEMI) (symbol->string $2)))
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
    (<expression> ((<infix-math>) $1))
    ;; Declaration
    (<declaration> ((ID = <expression>) `(set! ,$1 ,$3))
                   ((DEF ID = <expression>) `(define ,$2 ,$4))
                   
                   ((DEFER <expression>) `(_defer ,$2))
                   ((RECOVER LPAREN <expression> RPAREN <expression>)
                    `(_recover ,(list $3) ,$5))
                   ((ABORT <expression>) `(_abort ,$2))
                   ((BREAK) `(_break))
                   ((RETURN <expression>) `(_return ,$2))
                   )
    ;; Infix math
    (<infix-math> ((<structure-prec>) $1))
    
    ;; Lowest precedence: structures like if, while, etc
    (<structure-prec> ((IF <block-prec> <structure-prec>)
                       `(when ,$2 ,$3))
                      ((IF <block-prec> <structure-prec>
                           ELSE <structure-prec>)
                       `(if ,$2 ,$3 ,$5))
                      
                      ((FOR <block-prec> <structure-prec>)
                       `(let/ec _break (_while ,$2 ,$3)))
                      
                      ((FOR <structure-prec>)
                       `(let/ec _break (_while #t ,$2)))
                      
                      ((FOR ID TO <block-prec> <expression>)
                       `(for ([,$2 (in-range ,$4)]) ,$5))
                      
                      ((FUN LPAREN <id-comma-list> RPAREN
                            <structure-prec>)
                       `(lambda ,$3 ,$5))
                      
                      ((STRUCT LPAREN <id-comma-list> RPAREN)
                       `(_struct ,@$3))
                      
                      ((MAP ID IN <block-prec> <structure-prec>)
                       `((_member ,$4 Map)
                         (lambda(,$2) ,$5)))
                      
                      ((MAP ID IN <block-prec> WHERE <block-prec> <structure-prec>)
                       `((_member ((_member ,$4 Filter) (lambda (,$2) ,$6)) Map)
                         (lambda(,$2) ,$7)))
                      
                      ((YARN <structure-prec>) `(_yarn ,$2))
                      ((GUARD <structure-prec>) `(_guard ,$2))
                      ((MARK <structure-prec>)
                       `(let/ec _return
                          ,$2))
                      ((<block-prec>) $1))
    
    ;; Second precedence: blocks {...}
    (<block-prec> ((LBRACE <semi-list> RBRACE) `(let() ,@$2))
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
                       ((<plus-prec>) $1))
    (<plus-prec> ((<plus-prec> + <times-prec>) `(+ ,$1 ,$3))
                 ((<plus-prec> - <times-prec>) `(- ,$1 ,$3))
                 ((<plus-prec> ++ <times-prec>) `(_append ,$1 ,$3))
                 ((<times-prec>) $1))
    (<times-prec> ((<times-prec> * <unary-prec>) `(* ,$1 ,$3))
                  ((<times-prec> % <unary-prec>) `(modulo ,$1 ,$3))
                  ((<times-prec> / <unary-prec>) `(/ ,$1 ,$3))
                  ((<unary-prec>) $1))
    
    ;; Unary operators. Shitty shim currently.
    (<unary-prec> ((<funcall-prec>) $1))
    
    ;; Funcall-like things.
    (<funcall-prec> ((<funcall-prec> LPAREN <comma-list> RPAREN)
                     `(_funcall ,$1 ,@$3))
                    
                    ((<funcall-prec> LBRACK <expression> RBRACK)
                     `(_index ,$1 ,$3))
                    ((<funcall-prec> LBRACK <expression> -> <expression> RBRACK)
                     `(_index_set ,$1 ,$3 ,$5))
                    ((<funcall-prec> LBRACK <expression> COLON <expression> RBRACK)
                     `(_index_range ,$1 ,$3 ,$5))
                    
                    
                    ((<literal-prec> DOT ID)
                     `(_member ,$1 ,$3))
                    ((<literal-prec>) $1))
    
    (<literal-prec> ((ID) $1)
                    ((NUM) $1)
                    ((STR) $1)
                    ((LPAREN <expression> RPAREN) $2)
                    ((LBRACK <comma-list> RBRACK) (cons '_list $2))
                    ((LBRACK <expression> VBAR <expression> RBRACK)
                     `(_cons ,$2 ,$4)))
    
    )))

(define (string->ast x)
  (set! STRING x)
  (define chugger (open-input-string x))
  (define q (mir-parse (lambda () (mir-lex chugger))))
  q)

(define (map-spare-last fun lst)
  (define slst (take lst (sub1 (length lst))))
  (append (map fun slst) (drop lst (sub1 (length lst)))))

(define (pre-parse port)
  (define-values (in out) (make-pipe))
  (for ([i (in-lines port)])
    (match (string-trim i)
      ["" (void)]
      [(string-append (? (λ(x) (equal? "" (string-trim x)))) "//" rst) (void)]
      [(string-append (? (λ(x) (not (equal? "" (string-trim x)))) a) "//" rst)
       (displayln (semicolify a) out)]
      [i (displayln (semicolify i) out)]))
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

(provide (all-defined-out))
