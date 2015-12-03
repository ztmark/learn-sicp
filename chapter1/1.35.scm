
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


;(fixed-point (lambda (x) (+ (/ 1 x) 1)) 2.0)


;;;;;; 1.36
(define (print-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      ((if (close-enough? guess next)
          next
          (try next)))))
  (try first-guess))

;(print-fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)


;; 1.37

(define (cont-frac n d k)
  (define (cf n d y k)
    (if (= y k)
        (/ (n k) (d k))
        (/ (n y) (+ (d y) (cf n d (+ y 1) k)))))
  (cf n d 1 k))

(define (cont-frac-iter n d k)
  (define (iter n d k r)
    (cond ((= k 1) (/ (n 1) (+ (d 1) r)))
        (else (iter n d (- k 1) (/ (n k) (+ (d k) r))))))

  (iter n d (- k 1) (/ (n k) (d k))))

;; (/ 1.0 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))














