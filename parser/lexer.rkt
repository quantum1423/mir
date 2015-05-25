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
   HASH
   FUN WHILE IF ELSE BREAK MARK NAMESPACE IMPORT FOR
   ERROR DEFER RECOVER GUARD INTERFACE YARN TO DEF
   RETURN THIS INTO WHEN DO THEN
   RANGE WHERE UNSAFE COLLECT FROM
   OBJECT AND OR WAIT SEND RECV REPLY
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
      
      ((:or "fun"
            "while"
            "if"
            "else"
            "break"
            "mark"
            "namespace"
            "import"
            "defer" "from"
            "error"
            "guard"
            "interface"
            "reply"
            "recover"
            "yarn"
            "object"
            "then"
            "do"
            "for"
            "unsafe"
            "def"
            "return"
            "when"
            "and" "or" "not" "into"
            "range" "this"
            "where"
            "collect" "send" "recv"
            
            "=" ":=" "..." "++"
            "==" "===" "!=" "!=="
            "<" "<=" ">" ">=" "->" "<-"
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