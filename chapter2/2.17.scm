
;; (cons 1 (list 2 3 4 5)) -> (1 2 3 4 5)
;; (cons (list 1 2 3 4) 5) -> ((1 2 3 4) 5)
;; (cons (list 1 2 3) (list 4 5)) -> ((1 2 3) 4 5)
;; because (car (cons a b)) = a and car produce a s-expression
;; (cdr (cons a b)) = b and cdr produce a list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (length list1)
  (if (null? list1)
      0
      (+ 1 (length (cdr list1)))))

(define (last-pair list1)
  (if (= (length list1) 1)
      (car list1)
      (last-pair (cdr list1))))