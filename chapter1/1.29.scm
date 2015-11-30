

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
          (sum term (next a) next b))))

(define (cube a) (* a a a))
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (indentity x) x)

(define (sum-integers a b)
  (sum indentity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))

  (define (pi-next x)
    (+ x 4))

  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))

  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.29
;;;;;;;;;;;;;;
(define (even? n) (= (remainder n 2) 0))

(define (sim-integral f a b n)

  (define h (/ (- b a) n))

  (define (y k) (f (+ a (* k h))))

  (define (ff k)
    (cond ((or (= k 0) (= k n)) (y k))
        ((even? k) (* 2.0 (y k)))
        (else (* 4.0 (y k)))))

  (* (/ h 3.0)
      (sum ff 0 inc n)))


;;;;;;;;;;;;;
;1.30
;;;;;;;;;;;;;
(define (sum-in-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))



















