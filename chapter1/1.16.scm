


(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))

;;  b ^ n == (b ^ 2)^(n / 2)
;; 循环不变式 a * b^n
(define (fast-expt b n) 
    (define (fast-expt-iter b n a)
        (cond ((= n 0) a)
            ((even? n) (fast-expt-iter (square b) (/ n 2) a))
            ;; n为奇数的时候，多出来的1个b乘到a中
            ;; n最终都会变为1继而为0，所以a最终会是答案
            (else (fast-expt-iter (square b) (/ (- n 1) 2) (* a b)))))
    (fast-expt-iter b n 1))
