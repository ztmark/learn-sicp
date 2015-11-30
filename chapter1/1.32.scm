

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



(define (sum term a next b)
  (accumulate-in-iter + 0 term a next b))


(define (product a b next f)
  (accumulate-in-iter * 1 f a next b))