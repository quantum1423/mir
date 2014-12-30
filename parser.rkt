#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)
(require match-string)

(define-tokens value-tokens
  (NUM ID STR SYMBOL))
(define-empty-tokens syntax-tokens
  (EOF
   LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
   SEMI COMMA DOT COLON
   + - * / % .fl+. .fl-. .fl*. .fl/. ^ = :=
   === < != !== == <= >= > -> \\
   HASH
   FUN WHILE IF ELSE BREAK MARK NAMESPACE IMPORT FOR
   ABORT RAISE DEFER RECOVER GUARD INTERFACE YARN TO DEF
   RETURN GOTO DCAST SCAST IS 
   LTUP RTUP VBAR MAP IN WHERE UNION UNSAFE COLLECT
   NEW VOID AND OR
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
         "while"
         "if"
         "else"
         "break"
         "mark"
         "namespace"
         "import"
         "defer"
         "abort"
         "guard"
         "interface"
         "recover" "void"
         "yarn"
         "new"
         "for"
         "unsafe"
         "def"
         "to"
         "return"
         "union"
         "goto"
         "map"
         "dcast"
         "scast"
         "is"
         "in"
         "where"
         "collect"
         
         "="
         "==" "===" "!=" "!=="
         "<" "<=" ">" ">=" "->"
         "+" "-" "*" "\\" "/" "%") (string->symbol (string-upcase lexeme)))
   
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
   ("||" 'OR)
   ("&&" 'AND)
   ))

(define mangle-symbol
  (let ([x (gensym 'mangled)])
    (lambda (sym)
      (string->symbol
       (string-append "__" (symbol->string x) "__"
                      (symbol->string sym))))))

(define (mangle-with pfx sym)
  (string->symbol (string-append pfx (symbol->string sym))))

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
                 <semi-list>) `(_program "" ,$1 (_body ,@$2))))
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
                   
                   ((DEF ID <type-name> = <expression>) `(define: ,$2 : ,$3 ,$5))
                   
                   ((FUN ID LPAREN 
                         <arg-comma-list> RPAREN <type-name> <expression>) 
                    `(define: ,$2 : 
                       (-> ,@(map third $4) ,$6) (lambda: ,$4 : ,$6 ,$7)))
                   
                   ((DEF ID < <types-comma-list> > 
                         LPAREN <arg-comma-list> RPAREN <type-name> <expression>) 
                    `(begin
                       (: ,$2 (All ,$4 (-> ,@(map third $7) ,$9)))
                       (define ,$2 (lambda: ,$7 : ,$9 ,$10))))
                   
                   ((INTERFACE ID LBRACE <arg-semi-list> RBRACE)
                    `(_interface ,(mangle-with "%" $2) ,@$4))
                   
                   ((INTERFACE ID < <types-comma-list> > LBRACE <arg-semi-list> RBRACE)
                    `(_interface-poly ,$4 ,(mangle-with "%" $2) ,@$7))
                   
                   ((DEFER <expression>) `(_defer ,$2))
                   ((RECOVER LPAREN <expression> RPAREN <expression>)
                    `(_recover ,(list $3) ,$5))
                   ((ABORT <expression>) `(_abort ,$2))
                   ((BREAK) `((cast _break (-> Void))))
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
                      
                      ((FUN LPAREN <arg-comma-list> RPAREN <type-name>
                            <structure-prec>)
                       `(lambda: ,$3 : ,$5 ,$6))
                      
                      ((FOR ID IN <block-prec> <structure-prec>)
                       `(for ([,$2 (->sequence ,$4)]) ,$5))
                      
                      ((FOR ID IN <block-prec> WHERE <block-prec> 
                            <structure-prec>)
                       `(for ([,$2 (->sequence ,$4)]
                              #:when ,$6)
                          ,$7))
                      
                      ((FOR ID IN <block-prec> COLLECT LBRACK RBRACK <type-name> <structure-prec>)
                       `(for/slice (Vectorof ,$8) ([,$2 (->sequence ,$4)]) ,$9))
                      
                      ((FOR ID IN <block-prec> WHERE <block-prec> COLLECT LBRACK RBRACK <type-name>
                            <structure-prec>)
                       `(for/slice (Vectorof ,$10) ([,$2 (->sequence ,$4)] #:when ,$6)
                          ,$11))
                      
                      ((YARN <structure-prec>) `(_yarn ,$2))
                      ((GUARD <structure-prec>) `(_guard ,$2))
                      ((UNSAFE <structure-prec>) `(_unsafe ,$2))
                      ((MARK <structure-prec>)
                       `(let/ec _lol (define _break (cast _lol (-> Void)))
                          ,$2))
                      
                      ((NEW <type-name> LBRACE <semi-list> RBRACE)
                       `(_object ,$2 ,@$4))
                      
                      ((<block-prec>) $1))
    
    ;; Second precedence: blocks {...}
    (<block-prec> ((LBRACE <semi-list> RBRACE) `(let() ,@$2))
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
                     `(_funcall ,$1 ,@$3))
                    ((<funcall-prec> < <types-comma-list> >)
                     `(inst ,$1 ,@$3))
                    
                    ;; HAX! Fix for foo<int>(bar) parsing wrongly
                    ((<funcall-prec> < <type-name> > LPAREN <expression> RPAREN)
                     `(_funcall (inst ,$1 ,$3) ,$6))
                    
                    ((DCAST < <type-name> >)
                     `(lambda (x) (cast x ,$3)))
                    ((SCAST < <type-name> > LPAREN <expression> RPAREN)
                     `(ann ,$6 ,$3))
                    ((IS < <type-name> >)
                     `(make-predicate ,$3))
                    
                    ((<funcall-prec> LBRACK <expression> RBRACK)
                     `(_index ,$1 ,$3))
                    ((<funcall-prec> LBRACK <expression> COLON <expression> RBRACK)
                     `(_range ,$1 ,$3 ,$5))
                    
                    
                    ((<funcall-prec> DOT ID)
                     `(_member ,$1 ,$3))
                    ((<literal-prec>) $1))
    
    (<literal-prec> ((ID) $1)
                    ((NUM) $1)
                    ((STR) $1)
                    ((LPAREN <expression> RPAREN) $2)
                    ((LBRACK <comma-list> RBRACK) (cons '_list $2))
                    ((LTUP <comma-list> RBRACK) (cons 'vector $2)))
    
    
    
    ;; Type-related things
    (<arg-comma-list> ((ID <type-name> COMMA <arg-comma-list>)
                       (cons (list $1 ': $2) $4))
                      ((ID <type-name>) (list (list $1 ': $2)))
                      (() empty))
    (<arg-semi-list> ((ID <type-name> SEMI <arg-semi-list>)
                      (cons (list $1 ': $2) $4))
                     ((ID <type-name>) (list (list $1 ': $2)))
                     (() empty))
    (<types-comma-list> ((<type-name> COMMA <types-comma-list>) (cons $1 $3))
                        ((<type-name>) (list $1))
                        (() empty))
    
    (<type-name> ((ID) (string->symbol
                        (string-append "%" (symbol->string $1))))
                 ((VOID) '$void)
                 ((FUN LPAREN <types-comma-list> RPAREN <type-name>)
                  `(-> ,@$3 ,$5))
                 ((<type-name> < <types-comma-list> >)
                  `(,$1 ,@$3))
                 ((UNION < <types-comma-list> >)
                  `(U ,@$3))
                 ((LBRACK RBRACK <type-name>)
                  `($slice ,$3))
                 ((LTUP <types-comma-list> RBRACK)
                  `(Vector ,@$2)))
    
    )))

(define (string->ast x)
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