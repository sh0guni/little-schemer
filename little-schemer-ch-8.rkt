#lang racket
(require test-engine/racket-tests)
(require "little-schemer.rkt")

(define rember-f
  (λ (test?)
    (λ (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

(check-expect ((rember-f  =) '5 '(6 2 5 3)) '(6 2 3))
(check-expect ((rember-f  eq?) 'jelly '(jelly beans are good)) '(beans are good))
(check-expect ((rember-f  equal?) '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))

(define insertL-f
  (λ (test?)
    (λ (new old lat)
      (cond
        [(null? lat) '()]
        [(test? old (car lat)) (cons new lat)]
        [else (cons (car lat) ((insertL-f test?) new old (cdr lat)))]
        ))))

(check-expect ((insertL-f eq?) 'd 'e '(a b c e f g)) '(a b c d e f g))

(define seqL
  (λ (new old l)
    (cons new (cons old l))))

(define seqR
  (λ (new old l)
    (cons old (cons new l))))

(define insert-g
  (λ (seq)
    (λ (new old lat)
      (cond
        [(null? lat) '()]
        [(eq? old (car lat)) (seq new old (cdr lat))]
        [else (cons (car lat) ((insert-g seq) new old (cdr lat)))]
        ))))

(define insertL (insert-g seqL))
(check-expect (insertL 'd 'e '(a b c e f g)) '(a b c d e f g))
(define insertR (insert-g seqR))
(check-expect (insertR 'e 'd '(a b c d f g)) '(a b c d e f g))

(define seqS
  (λ (new old l)
    (cons new l)))

(define subst (insert-g seqS))
(check-expect (subst 'd 'e '(a b c e f g)) '(a b c d f g))

(define atom-to-function
  (λ (x)
    (cond
      [(eq? '+ x) +]
      [(eq? '× x) *]
      [else expt])))

(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (λ (aexp)
    (car aexp)))

(define value
  (λ (nexp)
    (cond
      [(atom? nexp) nexp]
      [else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))]
      )))

(check-expect (value 13) 13)
(check-expect (value '(+ 1 3)) 4)
(check-expect (value '(+ 1 (↑ 3 4))) 82)

(define multirember-f
  (λ (test?)
    (λ (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) ((multirember-f test?) a (cdr l))]
        [else (cons (car l) ((multirember-f test?) a (cdr l)))]))))

(define multirember-eq (multirember-f eq?))

(check-expect (multirember-eq 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
(check-expect (multirember-eq 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored jelly))
(check-expect (multirember-eq 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))

(define multiremberT
  (λ (test? lat)
    (cond
      [(null? lat) '()]
      [(test? (car lat)) ((multirember-f test?) (cdr lat))]
      [else (cons (car lat) ((multirember-f test?) (cdr lat)))])))

(define multiinsertLR
  (λ (new oldL oldR lat)
    (cond
      [(null? lat) '()]
      [(eq? oldL (car lat))
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? oldR (car lat))
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat))))]
      [else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))]
      )))

(define multiinsertLR&co
  (λ (new oldL oldR lat col)
    (cond
      [(null? lat) (col '() 0 0)]
      [(eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (add1 L) R))
                         )]
      [(eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L (add1 R)))
                         )]
      [else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R) (col (cons (car lat) newlat) L R))
                         )]
      )))

(check-expect (multiinsertLR&co 'x 'a 'b '() (λ (newlat L R) '(c))) '(c))
(check-expect (multiinsertLR&co 'x 'a 'b '() (λ (newlat L R) (cons L '()))) '(0))
(check-expect (multiinsertLR&co 'x 'a 'b '() (λ (newlat L R) (cons R '()))) '(0))
(check-expect (multiinsertLR&co 'x 'a 'b '(a b) (λ (newlat L R) '(c))) '(c))
(check-expect (multiinsertLR&co 'x 'a 'b '(a b) (λ (newlat L R) newlat)) '(x a b x))
(check-expect (multiinsertLR&co 'x 'a 'b '(a b) (λ (newlat L R) (cons L (cons R '())))) '(1 1))
(check-expect (multiinsertLR&co 'x 'a 'b '(a b c) (λ (newlat L R) (cons L (cons R '())))) '(1 1))
(check-expect (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (λ (newlat L R) (cons L (cons R '())))) '(2 2))
(check-expect (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (λ (newlat L R) newlat)) '(chips salty and salty fish or salty fish and chips salty))

(define evens-only*
  (λ (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(even? (car l)) (cons (car l) (evens-only* (cdr l)))]
         [else (evens-only* (cdr l))])]
      [else (cons (evens-only* (car l)) (evens-only* (cdr l)))]
      )))

(check-expect (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) '((2 8) 10 (() 6) 2))

(define evens-only*&co
  (λ (l col)
    (cond
      [(null? l) (col '() 1 0)]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (evens-only*&co
           (cdr l)
           (λ (newl p s)
             (col (cons (car l) newl) (* (car l) p) s)))]
         [else
          (evens-only*&co
           (cdr l)
           (λ (newl p s)
             (col newl p (+ (car l) s))))])]
      [else (evens-only*&co
             (car l)
             (λ (al ap as)
               (evens-only*&co
                (cdr l)
                (λ (dl dp ds)
                  (col (cons al dl) (* ap dp) (+ as ds))))))]
      )))

(check-expect
 (evens-only*&co
  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  (λ (newl product sum)
    (cons sum
          (cons product
                newl))))
 '(38 1920 (2 8) 10 (() 6) 2)
 )

(test)
