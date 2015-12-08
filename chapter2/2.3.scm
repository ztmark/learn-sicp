
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-retangle w h) (cons w h))
(define (width r) (car r))
(define (height r) (cdr r))

(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (length s)
  (if (= (x-point (start-segment s)) (x-point (end-segment s)))
      (abs (- (y-point (start-segment s)) (y-point (end-segment s))))
      (abs (- (x-point (start-segment s)) (x-point (end-segment s))))))

(define (circumference r) 
  (* 2 (+ (length (width r)) (length (height r)))))

(define (area r)
  (* (length (width r)) (length (height r))))






