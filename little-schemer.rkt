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

(define insertR
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))]
      )))

(check-expect (insertR 'e 'd '(a b c d f g)) '(a b c d e f g))
(check-expect (insertR 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))

(define insertL
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new lat)]
      [else (cons (car lat) (insertL new old (cdr lat)))]
      )))

(check-expect (insertL 'd 'e '(a b c e f g)) '(a b c d e f g))

(define subst
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))]
      )))

(check-expect (subst 'd 'e '(a b c e f g)) '(a b c d f g))

(define subst2
  (λ (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? o1 (car lat)) (eq? o2 (car lat)))
           (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))]
      )))

(check-expect (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
              (quote (vanilla ice cream with chocolate topping)))

(define multirember
  (λ (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))
 
(check-expect (multirember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
(check-expect (multirember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored jelly))
(check-expect (multirember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))

(define multiinsertR
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))]
      )))

(check-expect (multiinsertR 'e 'd '(a b c d f g)) '(a b c d e f g))
(check-expect (multiinsertR 'topping 'fudge '(fudge ice cream with fudge for dessert)) '(fudge topping ice cream with fudge topping for dessert))

(define multisubst
  (λ (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat) (multisubst new old (cdr lat)))]
      )))

(check-expect (multisubst 'd 'e '(a b c e f e g)) '(a b c d f d g))

(test)
