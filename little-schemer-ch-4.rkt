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
      [else (+ (car tup) (addtup (cdr tup)))]
      )))


(check-expect (addtup '(1 2 3)) 6)

(define ×
  (λ (n m)
    (cond
      [(zero? m) 0]
      [else (+ n (× n (sub1 m)))]
      )))

(check-expect (× 4 5) 20)
(check-expect (× 12 3) 36)

(define tup+
  (λ (tup1 tup2)
    (cond
      [(and (null? tup1) (null? tup2)) '()]
      [else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]
      )))

(check-expect (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
(check-expect (tup+ '(2 3) '(4 6)) '(6 9))

(test)