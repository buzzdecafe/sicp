(load "../base.scm")

; 1.1

; 1.2

(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))

(define (sum-sq-largest-2 x y z)
  (cond
    ((and (<= x y) (<= x z)) (sum-squares y z))  
    ((and (<= y z) (<= y z)) (sum-squares x z))
    ((and (<= z x) (<= z y)) (sum-squares x y))
   )
  )

(test "1.1 sum-sq-largest-2" 100 (sum-sq-largest-2 4 6 8))

