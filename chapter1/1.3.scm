

;;; 求三个数中较大的两个数的平方和
(define (sum-square a b c)

  (define (square a)
    (* a a))

  (define (square-sum a b)
    (+ (square a) (square b)))

  (cond ((and (<= a b) (<= a c)) (square-sum b c))
    ((and (<= b a) (<= b c)) (square-sum a c))
    ((and (<= c a) (<= c b)) (square-sum a b))))


