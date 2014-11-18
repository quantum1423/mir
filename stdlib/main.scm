(declare (block))
(declare (standard-bindings))

(define-macro (mir-fun a b)
             `(lambda ,(cdr a) ,b))

(define-macro (mir-if a b c d)
             `(if ,a ,b ,d))
             
(define-macro (mir-when a b)
              `(if ,a ,b (void)))

(define-macro (mir-time a) `(time ,a))

(define-macro (mir-yarn a)
  `(thread-start! (make-thread (lambda() (mir-guard ,a)))))
  
(define-macro (call a . rst)
  `(,a . ,rst))
 
  
(define-macro (mir-loop lname alst a)
  (define (proper-assoc x)
  (cond
    ((null? x) '())
    (else (cons (cons (car x) (cadr x))
                      (proper-assoc (cddr x))))))
  `(let ,lname ,(proper-assoc (cdr alst)) ,a))
  
(define (cur-yarn)
  (current-thread))
  
(define (recv)
  (thread-receive))
  
(define (send a b)
  (thread-send a b))
  
(define (zip a b)
  (cond
    ((null? a) '())
    (else (cons (cons (car a)
                      (car b))
                (zip (cdr a) (cdr b))))))
  
(define-macro (mir-struct . namez)
  (define (sunmangle x)
    (define startidx
      (let loop ((i 0))
        (cond
          ((equal? (string-ref x i) #\:) (+ i 2))
          (else (loop (+ 1 i))))))
    (substring x startidx (string-length x)))
    
  (define (unmangle x) (string->symbol (sunmangle (symbol->string x))))
  
  (define args (gensym))
  (define gaga (gensym))
  `(lambda ,args
    (define ,gaga (zip (quote ,(map unmangle namez)) ,args))
    (lambda (woo) (cdr (assoc woo ,gaga)))))
      
(define EOF #!eof)


(define current-defer-thunks (make-parameter '()))

(define-macro (mir-defer x)
  `(current-defer-thunks
    (cons (lambda() ,x) (current-defer-thunks))))
    
(define-macro (mir-guard x)
  `(parameterize ((current-defer-thunks '()))
    (with-exception-catcher
      (lambda (e) (for-each (lambda(x) (x)) (current-defer-thunks)) (raise e))
      (lambda()
        ,x
        (for-each (lambda(x) (x)) (current-defer-thunks))))))
