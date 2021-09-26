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

(define rember
  (λ (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(check-expect (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
(check-expect (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly))
(check-expect (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))

(define firsts
  (λ (l)
    (cond
      [(null? l) '()]
      [else (cons (car (car l)) (firsts (cdr l)))]
      )))

(check-expect (firsts '((a b) (c d) (e f))) '(a c e))
(check-expect (firsts '()) '())
(check-expect (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven))
(check-expect (firsts '(((five plums) (four)) (eleven green oranges) ((no) more))) '((five plums) eleven (no)))

(test)
