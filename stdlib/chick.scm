(define-syntax define-macro
(er-macro-transformer
(lambda (exp rename compare)
(if (symbol? (cadr exp))
(let ((name (cadr exp))
(expndr (caddr exp))
(-exp (gensym)) (-rename (gensym)) (-compare (gensym)))
`(define-syntax ,name
(er-macro-transformer (lambda (,-exp ,-rename ,-compare)
(apply ,expndr (cdr ,-exp))))))
(let ((name (caadr exp))
(formals (cdadr exp))
(body (cddr exp))
(-exp (gensym)) (-rename (gensym)) (-compare (gensym)))
`(define-syntax ,name
(er-macro-transformer (lambda (,-exp ,-rename ,-compare)
(apply (lambda ,formals ,@body) (cdr ,-exp))))))))))

(require-extension mailbox-threads)
(require-extension srfi-4)
(require-extension posix)
(use tcp)
(require-extension srfi-4-utils)
(require-extension byte-blob)

(define (tcp-listen-px prt) (tcp-listen prt))

(define (tcp-accept-px lst)
  (define-values (a b) (tcp-accept lst))
  (cons a b))
  
(define (tcp-close-px lst)
  (tcp-close lst))
  
(define (make-bytes n) (make-string n))
  
(define (read-bytes-avail bts port)
  (cond
    ((pair? port) ; TCP
                  (thread-wait-for-i/o! (port->fileno (car port)) #:input)
                  (define num (cadr (file-read (port->fileno (car port)) (string-length bts) bts)))
                  num)))
                  
(define (write-bytes bts port)
  (write-string bts #f (if (pair? port) (cdr port) port)))
  
(define (subbytes bts a b)
  (substring bts a b))
  
(define (close-port x)
  (cond
    [(input-port? x) (close-input-port x)]
    [(output-port? x) (close-output-port x)]
    [(pair? x) (close-port (car x)) (close-port (cdr x))]))
                      
(define (with-exception-catcher hand thnk)
  (condition-case (thnk)
    [var () (hand var)]))
    
(define (force-output x) (void))
