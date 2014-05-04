(load "exercises.scm")

(define (mul a b) (mul-iter 0 a b))

(define (mul-iter acc a b)
  (cond ((= 0 a) acc)
        ((= 0 b) acc)
        ((even? a) (mul-iter (+ acc :q
                                ) a b)


