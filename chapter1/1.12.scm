

;; Pascal’s triangle
(define (pascal-triangle row col)
    (cond ((or (< row 1) (< col 1)) 0)
        ((> col row) 0)
        ((or (= col 1) (= row col)) 1)
        (else (+ (pascal-triangle (- row 1) (- col 1))
                (pascal-triangle (- row 1) col)))))