
(define (smallest-divisor n)

  (define (smallest-divisor-iter n test)
    (cond ((> (* test test) n) n)
      ((not (= (remainder n test) 0)) 
        ;(smallest-divisor-iter n (+ test 1)))
        (smallest-divisor-iter n (next test)))
      (else test)))

  (smallest-divisor-iter n 2))

(define (next test-divisor)
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square n) (* n n))

;; (x * y) % m == ((x % m) * (y % m)) % m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  ;(if (prime? n)
  (if (fast-prime? n 10)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-for-primes lower-bound num)
  (cond ((= num 0) ())
      ((prime? lower-bound) 
        (timed-prime-test lower-bound)
        (newline)
        (search-for-primes (+ lower-bound 1) (- num 1)))
      (else (search-for-primes (+ lower-bound 1) num))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (test-carm n)
  
  (define (iter a)
    (cond ((>= a n) (display "pass"))
      ((= (expmod a n n) a)
        (iter (+ a 1)))
      (else (display "fail"))))

  (iter 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Miller-Rabin test

(define (mr-expmod base exp m)

  (define (square-check n)
    (if (and (= (remainder (square n) m) 1) 
              (not (= n 1))
              (not (= n (- m 1))))
        0
        (square n)))


  (cond ((= exp 0) 1)
    ((even? exp) (remainder 
                  (square-check (mr-expmod base (/ exp 2) m))
                  m))
    (else (remainder (* base (mr-expmod base (- exp 1) m)) m))))



(define (mr-test n)
  (define (try-it a)
    (= (mr-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-fast-prime? n times)
  (cond ((= times 0) true)
    ((mr-test n) (mr-fast-prime? n (- times 1)))
    (else false)))
















