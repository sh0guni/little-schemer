#lang racket
(require test-engine/racket-tests)

(define plus
  (λ (x y)
    (cond
      [(zero? y) x]
      [else (add1 (plus x (sub1 y)))]
      )))

(check-expect (plus 2 3) 5)

(define addtup
  (λ (tup)
    (cond
      [(null? tup) 0]
      [else (+ (car tup) (addtup (cdr tup)))])))


(check-expect (addtup '(1 2 3)) 6)

(test)