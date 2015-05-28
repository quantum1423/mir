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
   SEMI COMMA DOT COLON
   + - * / % .+ .- .* ./ ^ = := ++
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

(define mir-lex
  (lambda (in)
    (port-count-lines! in)
    ((lexer-src-pos
      ((eof) 'EOF)
      (#\# 'HASH)
      ((:: #\" (:* (:~ #\")) #\") (token-STR (string-parse lexeme)))
      ((:: "#\"" (:* (:~ #\")) #\") (token-BTS (string-parse (substring lexeme 1))))
      
      ((:or #\tab
            #\space
            #\newline) (return-without-pos (mir-lex input-port)))
      
      ((:or "fun" "if" "then" "for" "do" "import" "when" "end"
            
            "blk"
            
            "=" ":=" "..." "++"
            "==" "===" "!=" "!=="
            "<" "<=" ">" ">=" "->" "<-" "else"
            "+" "-" "*" "\\" "/" "%" ".+" ".-" ".*" "./") 
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
      ) in)))