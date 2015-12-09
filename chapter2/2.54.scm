

(define (my-equal? items1 items2)
  (cond ((and (null? items1) (null? items2)) #t)
      ((or (null? items1) (null? items2)) #f)
      ((and (not (pair? items1)) (not (pair? items2))) (eq? items1 items2))
      ((and (pair? items1) (pair? items2)) 
        (and (eq? (car items1) (car items2)) (my-equal? (cdr items1) (cdr items2))))
      (else #f)))