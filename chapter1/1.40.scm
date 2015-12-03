
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ((if (close-enough? guess next)
          next
          (try next)))))
  (try first-guess))

;;;
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))


(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


;;; 1.41
(define (double p)
  (lambda (x) (p (p x))))

;;;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


;;; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x) (f ((repeated f (- n 1)) x)))))


;;;;; 1.44
(define (t-average a b c)
  (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x) (t-average (f (- x dx)) (f x) (f (+ x dx)))))


(define (smooth-n f n)
  (repeated (smooth f) n))

;;;; 1.45
(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (nf f)
  (lambda (x) ((repeated average-damp 2) f) x))

(define (nrt g guess)
  (fixed-point (nf g) guess))



;;;; 1.46
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess) (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))


































