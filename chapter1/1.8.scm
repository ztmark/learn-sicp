

;; guess = (x / (guess * guess) + 2*guess) / 3


(define (square a) (* a a))

(define (divBy3 a b) (/ (+ a b) 3))

(define (cubert x)

    (define (good-enough? diff)
        (< (/ diff x) 0.00001))

    (define (improve guess)
        (divBy3 (/ x (square guess)) (* 2 guess)))

    (define (iter guess last-guess)
        (if (good-enough? (abs (- guess last-guess)))
            guess
            (iter (improve guess) guess)))

    (iter 1.0 x))