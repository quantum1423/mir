#lang racket
(require "parser.rkt")
(require graph)
(require racket/runtime-path)
(require natrium-crypt)
(provide (all-defined-out))

(define-runtime-path standard-lib
  "./stdlib/main.scm")

(define-runtime-path standard-macros
  "./stdlib/macros.scm")

(define-runtime-path library-root
  "./libraries/")

(define (fname->dir x)
  (define-values (a b c) (split-path x))
  a)

(define (vsystem thing)
  (displayln thing)
  (system thing))

(define (fname->cname fn)
  (string-append "/pscratch/.mir-cache/"
                 (bytes->hex
                  (sechash
                   (string->bytes/utf-8 (path-string->string fn))))
                 ".c"))


(define (impname->dir x base)
  (if (regexp-match #rx"\\.mir$" x)
      (path->complete-path x base)
      (string->path
       (string-append
        (path->string library-root)
        x
        "/main.mir"))))

(define (path-string->string x)
  (cond
    [(path? x) (path->string x)]
    [(path-string? x) x]
    [else (error 'path-string->string "WAT ~a" x)]))

(define (gather-dependencies fname)
  (define graph (directed-graph '()))
  (let rcr ([fname fname]
            [visited (set)])
    (unless (set-member? visited (path-string->string fname))
      (add-vertex! graph (~v (path-string->string fname)))
      (define prgm
        (with-input-from-file fname
          (lambda ()
            (string->ast (pre-parse (current-input-port))))))
      (match prgm
        [`(@program ,namespace
                    ,imports
                    ,body)
         (set! imports (cons "stdlib" imports))
         (when (equal? namespace '%%__RUNTIME__)
           (set! imports (cdr imports)))
         (for ([i (map (lambda (x) 
                         (impname->dir x (fname->dir fname))) imports)])
           (add-directed-edge! graph (~v 
                                      (path-string->string fname)) 
                               (~v (path-string->string i)))
           (rcr i (set-add visited (path-string->string fname))))])))
  (displayln "dependency graph:\n")
  (displayln (graphviz graph))
  graph)

(define (translate-file fname)
  (with-output-to-file "/tmp/last.scm"
    #:exists 'replace
    (lambda ()
      (define prgm
        (with-input-from-file fname
          (lambda ()
            (string->ast (pre-parse (current-input-port))))))
      (match prgm
        [`(@program ,namespace
                    ,imports
                    ,body)
         (define names
           (filter
            identity
            (for/list ([hoho body])
              (match hoho
                [`(define ,x . ,_)
                 (and (regexp-match #rx"^%%[A-Z]"
                                    (symbol->string x))
                      x)]
                [_ #f]))))
         (define shadows
           (filter
            identity
            (for/list ([hoho body])
              (match hoho
                [`(define ,x . ,_)
                 (and (not (regexp-match #rx"^%%[A-Z]"
                                         (symbol->string x)))
                      x)]
                [_ #f]))))
         (unless (equal? namespace '%%__RUNTIME__)
           (pretty-write `(namespace
                           (,(string-append (string-replace
                                             (symbol->string namespace)
                                             "%%" "") "#")
                            ,(gensym 'dummy)
                            ,@names)
                           (,(string-append (symbol->string (gensym 'private)) "#")
                            ,@shadows
                            ,(gensym 'dummy)))))
         (pretty-write `(include ,(path->string standard-macros)))
         (for ([hoho (cdr body)])
           (pretty-write hoho))]))))

(define (compile-file fname)
  (cond
    [(or (not (file-exists? (fname->cname fname)))
         (<= (file-or-directory-modify-seconds
              (fname->cname fname))
             (file-or-directory-modify-seconds
              fname)))
     (translate-file fname)
     (define stat
       (vsystem
        (format "/usr/local/Gambit-C/bin/gsc -c -o ~a /tmp/last.scm" 
                (fname->cname fname))))
     (if stat (fname->cname fname) (error "Compilation failed"))]
    [else (fname->cname fname)]))

(define (link-objects cnames outname)
  (vsystem
   (string-append (format "/usr/local/Gambit-C/bin/gsc -exe -o ~a " outname)
                  (string-join cnames " "))))

(define (build-from-file fname)
  (define graf (gather-dependencies fname))
  (unless (dag? graf)
    (display (graphviz graf))
    (error "Circular dependencies, cannot compile"))
  (define to-compile (for/list ([i (reverse (tsort graf))])
                       (read (open-input-string i))))
  (link-objects (for/list ([i to-compile])
                  (compile-file i))
                (string-replace (path-string->string fname) ".mir" "")))