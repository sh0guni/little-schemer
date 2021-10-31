#lang racket
(require test-engine/racket-tests)
(require "little-schemer.rkt")

(provide eqan)

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
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]
      )))

(check-expect (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
(check-expect (tup+ '(2 3) '(4 6)) '(6 9))
(check-expect (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))
(check-expect (tup+ '(4 6 8 1) '(3 7)) '(7 13 8 1))

(define gt
  (λ (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (gt (sub1 n) (sub1 m))]
      )))

(check-expect (gt 12 133) false)
(check-expect (gt 120 11) true)
(check-expect (gt 11 11) false)

(define lt
  (λ (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (lt (sub1 n) (sub1 m))]
      )))

(check-expect (lt 12 133) true)
(check-expect (lt 120 11) false)
(check-expect (lt 11 11) false)

(define eq
  (λ (n m)
    (nor (gt n m) (lt n m))
    ))

(check-expect (eq 2 2) true)
(check-expect (eq 2 3) false)
(check-expect (eq 3 2) false)

(define ↑
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (× n (↑ n (sub1 m)))]
      )))

(check-expect (↑ 1 1) 1)
(check-expect (↑ 2 3) 8)
(check-expect (↑ 5 3) 125)

(define len
  (λ (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (len (cdr lat)))]
      )))

(check-expect (len '(hotdogs with mustard sauerkratu and pickles)) 6)
(check-expect (len '(hamd and cheese on rye)) 5)

(define pick
  (λ (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))]
      )))

(check-expect (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)
(check-error (pick 0 '(a)))

(define no-nums
  (λ (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))]
      )))

(check-expect (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))

(define all-nums
  (λ (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))]
      )))

(check-expect (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))

(define eqan
  (λ (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(and (atom? a1) (atom? a2)) (eq? a1 a2)]
      [else #f]
      )))

(check-expect (eqan 3 3) #t)
(check-expect (eqan 2 3) #f)
(check-expect (eqan 'a 'a) #t)
(check-expect (eqan 'a 'b) #f)
(check-expect (eqan 1 'a) #f)

(define occur
  (λ (a lat)
    (cond
      [(null? lat) 0]
      [(eqan a (car lat)) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))]
      )))

(check-expect (occur 'e '(a b c e f e g)) 2)
(check-expect (occur 'd '(a b c e f e g)) 0)

(define one?
  (λ (n)
    (eqan 1 n)))
 
(check-expect (one? 1) #t)
(check-expect (one? 0) #f)
(check-expect (one? 4) #f)


(define rempick
  (λ (n lat)
    (cond
      [(one? n) (cdr lat)]
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))]
      )))

(check-expect (rempick 3 '(hotdog with hot mustard)) '(hotdog with mustard))

(test)
