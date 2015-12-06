
(define (length list1)
  (if (null? list1)
      0
      (+ 1 (length (cdr list1)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (reverse items)
  (if (= (length items) 1)
      items
      (append (reverse (cdr items)) (list (car items)))))

