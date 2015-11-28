

(define (square a) (* a a))

(define (average a b)
    (/ (+ a b) 2))


;; guess = (x + (x / guess)) / 2

(define (sqrt x)
    
    ;;(define (good-enough? guess)
        ;;(< (abs (- (square guess) x)) 0.001))


    (define (good-enough? diff)
        (< (/ diff x) 0.00001))


    (define (improve guess)
        (average guess (/ x guess)))

    (define (iter guess last-guess)
        (if (good-enough? (abs (- guess last-guess)))
            guess
            (iter (improve guess) guess)))

    (iter 1.0 x)
    )