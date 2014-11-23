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
    (else (cons (list (car x) (cadr x))
                      (proper-assoc (cddr x))))))
  `(let ,lname ,(proper-assoc (cdr alst)) ,a))
  
(define (cur-yarn)
  (current-thread))
  
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
(define current-recovers (make-parameter '()))

(define (attempt-recover e funs)
  (cond
    ((null? funs) (raise e))
    (else (let ([res ((car funs) e)])
            (if (not (equal? res '__prop__)) res (attempt-recover e (cdr funs)))))))
          
(define (propagate)
  '__prop__)

(define-macro (mir-defer x)
  `(current-defer-thunks
    (cons (lambda() ,x) (current-defer-thunks))))
    
(define-macro (mir-recover e x)
  `(current-recovers
    (cons (lambda ,(cdr e) ,x) (current-recovers))))
    
(define-macro (mir-guard x)
  `(parameterize ((current-defer-thunks '()))
    (with-exception-catcher
      (lambda (e) (for-each (lambda(x) (x)) (current-defer-thunks)) (attempt-recover e (current-recovers)))
      (lambda()
        ,x
        (for-each (lambda(x) (x)) (current-defer-thunks))))))
        
(define-macro (assign x y)
  `(begin
    (set! ,x ,y)
    ,x))
  
(define (make-semaphore n)
  (vector n (make-mutex) (make-condition-variable)))

(define (semaphore-wait! sema)
  (mutex-lock! (vector-ref sema 1))
  (let ((n (vector-ref sema 0)))
    (if (> n 0)
        (begin
          (vector-set! sema 0 (- n 1))
          (mutex-unlock! (vector-ref sema 1)))
        (begin
          (mutex-unlock! (vector-ref sema 1) (vector-ref sema 2))
          (semaphore-wait! sema)))))

(define (semaphore-signal-by! sema increment)
  (mutex-lock! (vector-ref sema 1))
  (let ((n (+ (vector-ref sema 0) increment)))
    (vector-set! sema 0 n)
    (if (> n 0)
        (condition-variable-broadcast! (vector-ref sema 2)))
    (mutex-unlock! (vector-ref sema 1))))
    
(define reply-sig (make-parameter #f))
  
(define (recv)
  (define haha (thread-receive))
  (reply-sig haha)
  (vector-ref haha 0))
  
(define (reply x)
  (define c (reply-sig))
  (vector-set! c 2 x)
  (semaphore-signal-by! (vector-ref c 1) 1))
  
(define (send a b)
  (define sm (make-semaphore 0))
  (define haha (vector b sm #f))
  (thread-send a haha)
  (semaphore-wait! sm)
  (vector-ref haha 2))
  
(define (dict-ref d v)
  (cond
    ((bytes? d) (bytes-ref d v))
    ((vector? d) (vector-ref d v))
    (else ((d 'Ref) v))))
    
(define-macro (mir-raise v) `(raise ,v))


;; Generic listy methods

(define (SMAppend a)
  (cond
    ((list? a) (lambda (x) (append a x)))
    ((vector? a) (lambda (x) (vector-append a x)))
    ((bytes? a) (lambda (x) (bytes-append a x)))
    ((string? a) (lambda (x) (string-append a x)))
    (else (a 'Append))))
    
(define (SMLength a)
  (cond
    ((list? a) (lambda () (length a)))
    ((vector? a) (lambda () (vector-length a)))
    ((bytes? a) (lambda () (bytes-length a)))
    ((string? a) (lambda () (string-length a)))
    (else (a 'Length))))
