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
(test "1.8 Newton's cube roots" #t (within-epsilon 3 (my-cube-root 27)))

; 1.9
; As described, the procedures are recursive:
; (+ 4 5)
; (inc (+ 3 5)
; (inc (inc (+ 2 5))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; ... likewise for dec version
;

; 1.10 Ackermann's function
;
(define (A x y) 
  (cond ((= y 0) 0) 
        ((= x 0) (* 2 y)) 
        ((= y 1) 2) 
        (else 
          (A (- x 1) (A x (- y 1)))))) 

(test "1.10 Ackermann 1 10" 1024 (A 1 10))
(test "1.10 Ackermann 2 4" 65536 (A 2 4))
(test "1.10 Ackermann 3 3" 65536 (A 3 3))

(define (f n) (A 0 n))
(test "1.10 f(n) -> 2n" 114 (f 57))
(define (g n) (A 1 n))
(test "1.10 g(n) -> 2^n" 32 (g 5))
(define (h n) (A 2 n))
(test "1.10 h(n) -> 2^{h(n-1)}" 65536 (h 4))


; 1.11
; f(n) = n if n < 3
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
(define (f-recursive n) (
                cond ((< n 3) n)
                      ((>= n 3) (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3)))))))


(define (f-iter n1 n2 n3 n) 
  (cond ((= n 0) n3)
        ((= n 1) n2)
        ((= n 2) n1)
        (else (f-iter (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (- n 1))))) 

(define (f n) (f-iter  2 1 0 n))
(test "1.11 tree recursion" 4489 (f 11))




