(load "../base.scm")

; 1.1.1 REPL fun
(test "1.1.1 interpreter result" 10 10)
(test "1.1.1 interpreter result" 12 (+ 5 3 4))
(test "1.1.1 interpreter result" 8 (- 9 1))
(test "1.1.1 interpreter result" 3 (/ 6 2))
(test "1.1.1 interpreter result" 6 (+ (* 2 4) (- 4 6)))

; 1.2 convert to prefix
(define val1.2 (/ 14.8 -60))
(test "1.2 convert to prefix" val1.2 
      (/
        (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
        (* 3 (- 6 2) (- 2 7))
              ))
; 1.1.2 
;(test "1.1.2 interpreter result" #<unspecified> (define a 3)) 
;(test "1.1.2 interpreter result" #<unspecified> (define a 3))
(define a 3)
(define b (+ a 1))
(test "1.1.2 interpreter result" 19 (+ a b (* a b)))
(test "1.1.2 interpreter result" #f (= a b))


; 1.3

(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))

(define (square-sum-larger x y z)
  (cond
    ((and (<= x y) (<= x z)) (sum-squares y z))  
    ((and (<= y z) (<= y z)) (sum-squares x z))
    ((and (<= z x) (<= z y)) (sum-squares x y))
   )
  )
(test "1.3 square-sum-larger" 100 (square-sum-larger 4 6 8))

; 1.4 
(define (mystery a b) ((if (> b 0) + -) a b)) 
; if b is greater than 0, then add a + b; 
; if b <= 0. then subtract a - b
; this is equivalent to a + (abs b) 
(define (absv x) (if (< x 0) (- x) x)) 
(test "1.4 mystery" 15 (mystery 10 5))
(test "1.4 mystery" 15 (mystery 10 -5))
(test "1.4 mystery" 10 (mystery 10 0))
(test "1.4 abs b" 15 (+ 10 (absv 5)))
(test "1.4 abs b" 15 (+ 10 (absv -5)))
(test "1.4 abs b" 10 (+ 10 (absv 0)))

;
; 1.5 normal vs applicative order
(define (p) (p)) 
(define (xtest x y) (if (= x 0) 0 y)) 

; Normal order: fully expand, then reduce
; (xtest 0 (p)) =>
;        (= x 0) => #t 
;                   done
; Since evaluation order is "lazy" we don't need to evaluate `p`

; Applicative order: evaluate the arguments, then apply
; (xtest 0 (p)) =>
;          eval p => p
;                    eval p => p
;                              eval p => p ...

; 1.6
(define (new-if predicate then-clause else-clause) (cond (predicate then-clause) (else else-clause))) 
; The example recurs infinitely. The expression is evaluated in aplicative order, and since
; the example `else-clause` calls `sqrt-iter`, it descends into that--that calls itself in its else-clause, and so 
; it goes.

; 1.7 
(define (average x y) (/ (+ x y) 2)) 
(define (improve guess x) (average guess (/ x guess))) 
(define (good-enough? old-guess new-guess) 
  (< (abs (- old-guess new-guess)) 0.001)) 
(define (sqrt-iter old-guess new-guess x) 
  (if (good-enough? old-guess new-guess) 
    new-guess 
    (sqrt-iter new-guess (improve new-guess x) x))) 
(define (my-sqrt x) (sqrt-iter 1.0 0.5 x))

(test "1.7 Newton's square roots" 0.0009 (square (my-sqrt 0.0009)))

; 1.8
; Newton's method for cube roots
(define (approx-cube x y) (/ (+ (/ x (square y)) (* 2 y)) 3))
(define (cube-iter old-guess new-guess x) 
  (if (good-enough? old-guess new-guess) 
    new-guess 
    (cube-iter new-guess (approx-cube x new-guess) x))) 
(define (my-cube-root x) (cube-iter 1.0 0.5 x))

(define epsilon 0.0000001)
(define (within-epsilon x y) (< (abs (- x y)) epsilon))
(test "1.7 Newton's cube roots" #t (within-epsilon 3 (my-cube-root 27)))

