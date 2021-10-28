#lang racket
(require test-engine/racket-tests)
(require "little-schemer.rkt")

(define rember*
  (λ (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) a) (rember* a (cdr l))]
         [else (cons (car l) (rember* a (cdr l)))])]
      [else (cons (rember* a (car l)) (rember* a (cdr l)))]
      )))
       

(check-expect (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick))))
(check-expect (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
                      '(((tomato)) ((bean)) (and ((flying)))))
(check-expect (rember* 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))

(define insertR*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons old (cons new (cdr l)))]
         [else (cons (car l) (insertR* new old (cdr l)))])]
      [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))]
      )))

(check-expect (insertR* 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
(check-expect (insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
              '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))

(define occur*
  (λ (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? a (car l)) (add1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))]
         )]
      [else (+ (occur* a (car l)) (occur* a (cdr l)))]
      )))

(check-expect (occur* 'banana
                      '((banana)
                        (split ((((banana ice)))
                                (cream (banana))
                                sherbet))
                        (banana)
                        (bread)
                        (banana brandy)))
              5)

(define subst*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons new (subst* new old (cdr l)))]
         [else (cons (car l) (subst* new old (cdr l)))]
         )]
      [else (cons (subst* new old (car l)) (subst* new old (cdr l)))]
      )))

(check-expect (subst* 'orange 'banana
                      '((banana)
                        (split ((((banana ice)))
                                (cream (banana))
                                sherbet))
                        (banana)
                        (bread)
                        (banana brandy)))
              '((orange)
                        (split ((((orange ice)))
                                (cream (orange))
                                sherbet))
                        (orange)
                        (bread)
                        (orange brandy)))

(define insertL*
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons new (cons old (cdr l)))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))]
      )))

(check-expect (insertL* 'pecker 'chuck
                        '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))
              '((how much (wood))
                          could
                          ((a (wood) pecker chuck))
                          (((pecker chuck)))
                          (if (a) ((wood pecker chuck)))
                          could pecker chuck wood))

(define member*
  (λ (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (or
         (eq? a (car l))
         (member* a (cdr l)))]
      [else (or (member* a (car l)) (member* a (cdr l)))]
      )))

(check-expect (member* 'chips '((potato) (chips ((with) fish) (chips)))) #t)

(test)