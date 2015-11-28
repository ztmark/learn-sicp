
;;; exercise 1.11
;;; f(n) = n (n < 3)
;;; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) (n >= 3)



;;; 递归计算, (f 30) 需要24秒左右
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))



;; 迭代计算
(define (fiter a b c i n)
    (cond ((= n 0) a)
        ((= n 1) b)
        ((= n 2) c)
        (else 
            (if (>= i n)
              (+ c (* 2 b) (* 3 a))
              (fiter b c 
                    (+ c (* 2 b) (* 3 a))
                    (+ i 1)
                    n)))))


(define (ff n)
  (fiter 0 1 2 3 n))