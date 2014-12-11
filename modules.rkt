#lang racket
(require match-string)
(require "parser.rkt")
(require racket/runtime-path)

#|
DESCRIPTION:

Each directory has a module. The name of the module is declared at the top of the program:

module: name
...

It's an error to attempt compilation of several files as a module, if the module names clash.

Importing a module goes like:

import: "path-to-module"

The uppercase bindings of the module at the path are then available as modname::ExportedBinding, etc.

The module declaration must be the first expression in the program. Import declaration must also be before any code.


IMPLEMENTATION:

We mangle all variable definitions in module "name" from "def" to "name::def". We mangle all references to variables from "var" to "name::var", unless the reference is already of the form "othername::Var". If we have "othername::notUpperCaseVar", then we throw an error.

The dependency graph must be a DAG. We topologically sort the graph, then concatenate each of the programs, with proper mangling, into one gigantic Scheme file. Only then do we add headers that define the macros etc.

This ugly approach allows maximal flexibility in choosing Chicken vs. Gambit Scheme. If Mir ever chooses a "canonical" Scheme->C compiler, we might do something faster. This dumb approach generates a very large amount of code, and causes slow compile times. It does ensure, though, that no runtime dependencies exist.
|#

(define dont-mangle
  (set "else" "length"
       "append" "cons" "car" "cdr"
       "send" "recv" "reply" "void" "false" "true"))

(define special-member
  (set "Length" "Append" "Map" "Filter"))

(define (mangle sexp prefix)
  (define rcr (lambda(x) (mangle x prefix)))
  (match sexp
    ['this `(this)]
    [`(quote . ,rst) sexp]
    [`(lambda ,x ,y) `(lambda ,(map rcr x)
                        ,(rcr y))]
    [`(_recover ,x ,y) `(_recover ,(map rcr x)
                                  ,(rcr y))]
    [`(_member ,a ,b) `(_member ,(rcr a) ,b)]
    [`(_struct . ,rst) sexp]
    [`(,macname . ,rst) `(,macname . ,(map rcr rst))]
    [(? symbol? s)
     (match (symbol->string s)
       [(string-append "scheme::" name) (string->symbol 
                                         (string-replace
                                          (string-replace
                                           (string-replace name "_" "-")
                                          "P" "?") "M" "!"))]
       [(string-append mod "::" bind)
        (unless (char-upper-case? (string-ref bind 0))
          (error (format "Cannot import lowercase identifiers! ~a" s)))
        s]
       [(string-append "_" bind) sexp]
       [(? (λ(x) (set-member? dont-mangle x)) q) s]
       [x (string->symbol
           (string-append prefix "::" x))])]
    [x x]))

(define (postproc ast)
  (match ast
    [`(let () . ,rst) (define alldefs
                        (filter identity
                                 (for/list ([el rst])
                                   (match el
                                     [`(_let ,a ,b) a]
                                     [_ #f]))))
                      `(let ,(for/list ([x alldefs])
                               (list x '(void)))
                         ,@(for/list ([x rst])
                            (match x
                              [`(_let ,a ,b) `(set! ,a ,(postproc b))]
                              [x (postproc x)])))]
    [`(_namespace ,x) ast]
    [`(_import ,x) ast]
    [(cons a b) (map postproc ast)]
    [x x]))

(define (get-metadata _sexp)
  (match _sexp
    [`(_program ,name ,imports ,bdd)
     (values name (postproc (mangle bdd name))
             imports)]))


;; Memoized interface to filenames
(define get-file-data
  (let ([c (make-hash)])
    (lambda (x)
      (cond
        [(hash-has-key? c x) (apply values (hash-ref c x))]
        [else (define ast (string->ast (read-src-file x)))
              (define-values (n m i) (get-metadata ast))
              (hash-set! c x (list n m i))
              (get-file-data x)]))))

(define (list-union a b)
  (define q (list->set a))
  (append a
          (filter (λ(x) (not (set-member? q x))) b)))

(define (list-union* x)
  (cond
    [(empty? x) empty]
    [else (list-union (car x) (list-union* (cdr x)))]))

(define (fname->dir x)
  (define-values (a b c) (split-path x))
  a)

(define (build-dependencies file)
  (define-values (name mangled imports) (get-file-data file))
  (cond
    [(empty? imports) (list file)]
    [else (define children 
            (parameterize ([current-directory (fname->dir file)])
            (map build-dependencies
                 (map (λ(x) (path->complete-path 
                             (exppath x)
                             (fname->dir file)))
                      imports))))
          (append (list-union* children) (list file))]))

(define-runtime-path chicklib-loc "./stdlib/chick.scm")
(define-runtime-path gamblib-loc "./stdlib/gambit.scm")
(define-runtime-path mirstdlib-loc "./stdlib/main.scm")

(define-runtime-path mirlibs-loc "./stdlib/mir")

(define (get-tmp)
  (string-replace (path->string (make-temporary-file))
                  "-" "__"))

(define (exppath x)
  (unless (or (regexp-match #rx"\\./" x)
              (regexp-match #rx"^~~" x))
    (set! x (string-append "~~/" x)))
  (set! x (string-replace x "~~" (path->string mirlibs-loc)))
  (cond
    [(equal? x (string-replace x ".mir" "cthulhu"))
     (cond
       [(file-exists? (string-append x ".mir"))
        (string-append x ".mir")]
       [(directory-exists? x)
        (string-append x "/main.mir")])]
    [else x]))

(define USE_CHICKEN #t)

(define (build-file file (output (string-replace file ".mir" "")))
  (set! file (normalize-path file))
  (define deps (build-dependencies file))
  (define-values (o buff) (make-pipe))
  
  (when USE_CHICKEN
    (write `(include ,(path->string chicklib-loc)) buff)
    (newline buff))
  
  (unless USE_CHICKEN
    (write `(include ,(path->string gamblib-loc)) buff)
    (newline buff))
  
  (write `(include ,(path->string mirstdlib-loc)) buff)
  (newline buff)
  (for ([el deps])
    (define-values (name mangled imports) (get-file-data el))
    (fprintf buff "\n;; ~a\n" (path->string el))
    (for-each (λ(x) (pretty-write x buff)
                (newline buff)) (cdr mangled)))
  (close-output-port buff)
  (define xaxa (get-tmp))
  (with-output-to-file xaxa
    #:exists 'truncate/replace
    (lambda ()
      (copy-port o (current-output-port))))
  
  (with-input-from-file xaxa
    (lambda()
      (copy-port (current-input-port) (current-output-port))))
  
  (cond
    [USE_CHICKEN
     (system
      (format "csc -stack-size 256K -optimize-leaf-routines -block -inline -inline-global -no-trace -no-lambda-info -lfa2 -o \"~a\" \"~a\"" output xaxa))]
    [else
     (system
      (format "gsc -exe -o \"~a\" \"~a\"" output xaxa))])
  (delete-file xaxa))

(provide build-file)
