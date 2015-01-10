(declare (block))
(declare (standard-bindings))
				 
(define (fatal str)
  (print "FATAL RUNTIME ERROR: ")
  (print str)
  (newline)
  (exit 42))
				 
(define-macro (@redef a b)
  `(set! ,a ,b))

(define-macro (@funcall a . b)
  `(,a . ,b))
  
(define-macro (%%__SCHEME x)
  (read (open-input-string x)))
  
;; Data-related things
  
(define-macro (@object . definitions)
	(define (filter pred lst)
		(cond
			[(null? lst) '()]
			[(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
			[else (filter pred (cdr lst))]))
  `(let()
    ,@definitions
      (lambda (VAL) 
        (case VAL
        ,@(map
		      	(lambda (def)
		          (define name (cadr def))
		          `((,name) ,name))
	          (filter
	          	(lambda (def)
	          		(and
	          			(equal? 'define (car def))
	          			(char>=? (string-ref (symbol->string (cadr def)) 2) #\A)
	          			(char<=? (string-ref (symbol->string (cadr def)) 2) #\Z)))
        			definitions))
			    (else 'not-found)))))
			
(define-macro (@member a b)
	`(let ([x (@funcall ,a (quote ,b))])
		(if (eq? x 'not-found) (error "Member not found") x)))
		
(define-macro (@range a b c)
  `(%%__range ,a ,b ,c))
  
(define-macro (@index a b)
  `(%%__index ,a ,b))
		
		
;; Control-flow
		
(define-macro (@while pred body)
	`(let loop()
		(when ,pred
			,body
			(loop))))
			
(define-macro (when a . b)
	`(if ,a (let() . ,b)))
	
(define-macro (let/ec k . rst)
	`(call/cc (lambda(,k) . ,rst)))
	
(define-macro (@yarn body)
	`(thread-start! (make-thread (lambda()  ,body))))
	
(define-macro (@unsafe body)
  `(let()
    (declare (not safe))
    (declare (fixnum))
    ,body))
    
(define-macro (define-values reses expr)
  (cond
    [(equal? 1 (length reses)) `(define ,(car reses) ,expr)]
    [else (error "Cannot return multiple values. Yet.")]))
