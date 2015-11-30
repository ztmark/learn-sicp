


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      (combiner null-value null-value)
      (combiner 
        (term a) 
        (accumulate combiner null-value term (next a) next b))))


(define (accumulate-in-iter combiner null-value term a next b)
  
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner (term x) result))))

  (iter a null-value))

(define (filter-accumulate combiner null-value term a next b filter)  
  (cond ((> a b) (combiner null-value null-value))
      ((filter a) (combiner
                    (term a)
                    (filter-accumulate combiner null-value term (next a) next b filter)))
      (else (combiner 
                null-value
                (filter-accumulate combiner null-value term (next a) next b filter)))))



(define (smallest-divisor n)

  (define (next test-divisor)
    (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

  (define (smallest-divisor-iter n test)
    (cond ((> (* test test) n) n)
      ((not (= (remainder n test) 0)) 
        ;(smallest-divisor-iter n (+ test 1)))
        (smallest-divisor-iter n (next test)))
      (else test)))

  (smallest-divisor-iter n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-sum a b)
  (define (identity x) x)
  (filter-accumulate + 0 identity a inc b prime?))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (prod n)
  (define (identity x) x)
  (define (filter x) (= (gcd n x) 1))
  (filter-accumulate * 1 identity 1 inc (- n 1) filter))






















