(load "../base.scm")

; 1.1 REPL fun
;
; 1.2 convert to prefix
(define val1.2 (/ 14.8 -60))
(test "1.2 convert to prefix" val1.2 
      (/
        (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
        (* 3 (- 6 2) (- 2 7))
              ))

; 1.3

(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))

(define (sum-sq-largest-2 x y z)
  (cond
    ((and (<= x y) (<= x z)) (sum-squares y z))  
    ((and (<= y z) (<= y z)) (sum-squares x z))
    ((and (<= z x) (<= z y)) (sum-squares x y))
   )
  )

(test "1.3 sum-sq-largest-2" 100 (sum-sq-largest-2 4 6 8))

