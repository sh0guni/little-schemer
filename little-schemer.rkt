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

(define member?
  (λ (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat)) (member? a (cdr lat)))])))

(check-expect (member? 'tea '(coffee tea or milk)) #t)
(check-expect (member? 'poached '(fried eggs and scrambled eggs)) #f)

(test)
 