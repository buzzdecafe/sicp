(load "exercises.scm")

; 1.17 squares by newton
(define (average x y)
  (/ (+ x y) 2))

;A guess is improved by averaging it with the quotient of the radicand and the old guess:
(define (improve guess x)
  (average guess (/ x guess)))

(define tolerance 0.001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance))

(define (square-iter guess x)
  (if (good-enough? guess x)
    guess
    (square-iter (improve guess x) x)))

(define (sqrt x)
  (square-iter 1.0 x))

