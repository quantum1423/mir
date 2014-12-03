(declare (standard-bindings))
(declare (extended-bindings))
(declare (block))

(define printf
  (lambda (fmtstr . args)
    (let ((len (string-length fmtstr)))
      (let loop ((i 0) (args args))
        (let ((output
                (lambda (fn)
                  (fn (car args))
                  (loop  (+ i 2) (cdr args))))
              (outputc
                (lambda (fn)
                  (fn)
                  (loop (+ i 2) args))))
          (if (>= i len) #t
            (let ((c (string-ref fmtstr i)))
              (if (char=? c #\~)
                (case (string-ref fmtstr (+ i 1))
                  ((#\s) (output write))
                  ((#\a) (output display))
                  ((#\c) (output write-char))
                  ((#\% #\n) (outputc newline))
                  ((#\~) (outputc (lambda () (write-char #\~))))
                  (else
                    (write
                      "error in eopl:printf: unknown fmtstr character ")
                    (write-char  (string-ref fmtstr (+ i 1)))
                    (write-char #\newline)
                    (error "WTF!")))
                (begin
                  (display c)
                  (loop (+ i 1) args))))))))))
                  
(define-macro (when a b)
  `(if ,a ,b ,(void)))
  
  
(define (bound? name)
  (not (##unbound? (##global-var-ref (##make-global-var name)))))
                  
                  
(define (format . argz)
  (with-output-to-string '()
    (lambda()
      (apply printf argz))))
      
(define (tcp-listen-px prt)
  (open-tcp-server (list port-number: prt
                         backlog: 2048)))
  
(define (tcp-accept-px lst)
  (read lst))

(define (tcp-connect-px host port)
  (open-tcp-client (list
                      server-address: host
                      port-number: port
                      coalesce: #f
                      keep-alive: #t)))
  
(define (tcp-close-px lst)
  (close-port lst))
  
(define (make-bytes n)
  (make-u8vector n))
  
(define (read-bytes-avail bts port)
  (read-subu8vector bts 0 (u8vector-length bts) port 1))
  
(define (write-bytes bts port)
  (write-subu8vector bts 0 (u8vector-length bts) port))
  
(define (bytes-length x)
  (u8vector-length x))
  
(define bytes u8vector)
(define bytes? u8vector?)

(define bytes-ref u8vector-ref)
(define bytes-set! u8vector-set!)
  
(define subbytes subu8vector)

(define bytes-append u8vector-append)

;; Horrible, slow hack :(
(define (bytes2string bts)
  (list->string
    (map integer->char
      (u8vector->list bts))))
      
(define UNBOUND #!unbound)

(define-macro (assign x y)
  `(begin
    (when (##unbound? ,x) (mir-panic (format "Cannot assign to unbound variable! ~a <- ~a" ,x ,y)))
    (set! ,x ,y)
    ,x))
    
(define (exn-message ex)
  (cond
    ((error-exception? ex) (error-exception-message ex))
    (else ex)))
