
;(define (* a b)
;  (if (= b 0)
;    0
;    (+ a (* a (- b 1)))))

; 这里千万别用 (* a 2)
(define (double a) (+ a a))

; a is even
(define (halve a) (/ a 2))

(define (even? n) (= (remainder n 2) 0))

(define (* a b)
  
  (define (iter a b c)
    (cond ((= b 0) c)
      ((even? b) (iter (double a) (halve b) c))
      (else (iter (double a) (halve (- b 1)) (+ c a)))))

  (iter a b 0))