
(define (gcd n m)
  (define (int-gcd b s)
    (if (= s 0)
        b
        (int-gcd s (remainder b s))))
  (if (> n m)
      (int-gcd n m)
      (int-gcd m n)))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((or (and (> n 0)  (> d 0)) (and (< n 0) (> d 0))) (cons (/ n g) (/ d g)))
        (else (cons (- (/ n g)) (- (/ d g)))))))
