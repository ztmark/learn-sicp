
(define (p) (p))

(define (test x y)
    (if (= x 0)
        0
        y))

;; 应用序会在 p 方法中无限循环
;; 正则序将输出0
