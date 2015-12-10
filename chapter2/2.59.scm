
(define (element-of-set? x set)
  (cond ((null? set) #f)
      ((equal? x (car set)) #t)
      (else (element-of-set? x (cdr set)))))


(define (adjoin x set)
  (cond ((element-of-set? x set) set)
      (else (cons x set))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
      ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set? (cdr set1) set2)))
      (else (intersection-set? (cdr set1) set2))))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
      ((null? set2) set1)
      ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
      (else  (cons (car set1) (union-set (cdr set1) set2)))))