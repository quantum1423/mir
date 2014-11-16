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

(use mailbox-threads)
