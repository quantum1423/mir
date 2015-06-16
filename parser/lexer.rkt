#lang racket/base
(require parser-tools/lex
         racket/string)
(require (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))


(define-tokens value-tokens
  (INT ID STR SYMBOL BTS))
(define-empty-tokens syntax-tokens
  (EOF  
   LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
   SEMI COMMA DOT COLON __RSPLICE__ LET
   + - * / % :+ :- :* :/ ^ = := ++
   === < != !== == <= >= > -> \\  <-
   FUN FOR DO BLK
   IF THEN ELSE
   END
   
   IMPORT
   ...
   ))

(define (string-parse x)
  (set! x (substring x 1 (sub1 (string-length x))))
  (string-replace x "\\n" "\n"))

(define LAST #f)

(define mir-lex
  (lambda (in)
    #;(when LAST
      (write (token-name (position-token-token LAST)))
      (newline))
    (port-count-lines! in)
    (define T
      ((lexer-src-pos
      ((eof) 'EOF)
      (#\# 'HASH)
      ((:: #\" (:* (:~ #\")) #\") (token-STR (string-parse lexeme)))
      ((:: "#\"" (:* (:~ #\")) #\") (token-BTS (string-parse (substring lexeme 1))))
      
      ((:or #\tab
            #\space) (return-without-pos (mir-lex input-port)))
      
      ((:: #\newline) (if (and LAST (not
                                     (member (token-name (position-token-token LAST))
                                             '(LBRACE LPAREN LBRACK SEMI))))
                          'SEMI
                          (return-without-pos (mir-lex input-port))))
      
      ((:or "fun" "if" "then" "for" "do" "import" "when" "end"
            "let"
            
            "blk"
            
            "=" ":=" "..." "++"
            "==" "===" "!=" "!=="
            "<" "<=" ">" ">=" "->" "<-" "else" "__rsplice__"
            "+" "-" "*" "\\" "/" "%" ":+" ":-" ":*" ":\\") 
          (string->symbol (string-upcase lexeme)))
      
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
                     (:/ #\0 #\9)))
               (:or "" "?" "!"))) (token-ID 
                                   (string->symbol lexeme)))
      
      
      ((:: (:+ (:or (:/ #\0 #\9)))) (token-INT (string->number lexeme)))
      ((:: (:+ (:/ #\0 #\9))
           #\.
           (:+ (:/ #\0 #\9))) (token-INT (string->number lexeme)))
      
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
      ) in))
    (set! LAST T)
    T))