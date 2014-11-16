#lang racket
(require "parser.rkt")
(require racket/runtime-path)

(define-runtime-path chicklib-loc "./stdlib/chick.scm")
(define-runtime-path mirstdlib-loc "./stdlib/main.scm")

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

(define (string->ast x)
  (define chugger (open-input-string x))
  (mir-parse (lambda () (mir-lex chugger))))

(match (current-command-line-arguments)
  [(vector "trans" fname)
   (write `(include ,(path->string chicklib-loc)))
   (newline)
   (write `(include ,(path->string mirstdlib-loc)))
   (newline)
   (newline)
   (define x (read-src-file fname))
   (write (string->ast x))
   (newline)])