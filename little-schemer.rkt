#lang racket
(require test-engine/racket-tests)

(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))

(check-expect (atom? (quote ())) #f)

(define lat?
  (λ (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(check-expect (lat? (quote (Jack Sprat could eat no chicket fat))) #t)
(check-expect (lat? (quote ((Jack) Sprat could eat no chicket fat))) #f)
(check-expect (lat? (quote (Jack (Sprat could) eat no chicket fat))) #f)
(check-expect (lat? '()) #t)
(test)
 