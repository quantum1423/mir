(declare (block))
(declare (standard-bindings))

(define-macro (mir-fun a b)
             `(lambda ,(cdr a) ,b))

(define-macro (mir-if a b c d)
             `(if ,a ,b ,d))

(define-macro (mir-time a) `(time ,a))

(define-macro (mir-yarn a)
  `(thread-start! (make-thread (lambda() ,a))))
  
(define-macro (call a . rst)
  `(,a . ,rst))
  
(define-macro (mir-loop a)
  `(let loop() ,a))
  
(define (cur_yarn)
  (current-thread))
  
(define (recv)
  (thread-receive))
  
(define (send a b)
  (thread-send a b))
