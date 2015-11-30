


(define (inc a) (+ a 1))

(define (product a b next f)
  (if (> a b)
      1
      (* (f a) (product (next a) b next f))))

(define (product-in-iter a b next f)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))


(define (factorial n)
  (define (identity x) x)
  (product-in-iter 1 n inc identity))

;;;;;;;;;;;;;;
(define (pi n)
  
  (define (next a) (+ a 2))

  (define (f1 a) (/ a (inc a)))
  (define (f2 a) (/ a (- a 1)))

  (define n1
    (if (even? n)
        (/ n 2)
        (/ (+ n 1) 2)))
  (define n2
    (if (even? n)
        (/ n 2)
        (/ (- n 1) 2)))
  (define (end s k) (+ s (* 2 (- k 1))))

  (* 4.0 
    (product-in-iter 2 (end 2 n1) next f1)
    (product-in-iter 4 (end 4 n2) next f2)))


