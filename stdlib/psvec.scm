
;; A Braun tree is a vector of four elements: A S T N

;; This is obviously not the most efficient pvector ever

(define-structure Leaf val)
(define-structure Node val left right)
(define-structure RList treelist)

(define (pvec-cons x vk)
  (define vec (RList-treelist vk))
  (cond
    [(or (null? vec) (null? (cdr vec)))
     (make-RList (cons (cons 1 (make-Leaf x)) vec))]
    [else (let* ([first (car vec)]
                 [rest (cdr vec)]
                 [weight1 (car first)]
                 [weight2 (car (car rest))])
            (cond
              [(eq? weight1 weight2)
               (make-RList (cons (cons (+ 1 weight1 weight2)
                                       (make-Node x (cdr first) (cdr (car rest))))
                                 (cdr rest)))]
              [else (make-RList (cons (cons 1 (Leaf x)) vec))]))]))
              

