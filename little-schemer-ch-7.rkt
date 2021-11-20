#lang racket
(require test-engine/racket-tests)
(require "little-schemer.rkt")

(define set?
  (λ (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(check-expect (set? '(apple peaches plum)) #t)
(check-expect (set? '(apple peaches apple plum)) #f)
(check-expect (set? '(apple 3 pear 4 9 apple 3 4)) #f)

(define makeset
  (λ (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))])))

(check-expect (makeset '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))

(define subset?
  (λ (set1 set2)
    (cond
      [(null? set1) #t]
      [else (and (member? (car set1) set2) (subset? (cdr set1) set2))]
      )))

(check-expect (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) #t)
(check-expect (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)) #f)

(define eqset?
  (λ (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(check-expect (eqset? '(6 large chickens with wings) '(6 chickens with large wings)) #t)

(define intersect?
  (λ (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2) (intersect? (cdr set1) set2))]
      )))

(check-expect (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)) #t)

(define intersect
  (λ (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)]
      )))

(check-expect (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))

(define union
  (λ (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) (union (cdr set1) set2)]
      [else (cons (car set1) (union (cdr set1) set2))]
      )))

(check-expect (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese)) '(stewed tomatoes casserole macaroni and cheese))

(define intersectall
  (λ (l-set)
    (cond
      [(null? (cdr l-set)) (car l-set)]
      [else (intersect (car l-set) (intersectall (cdr l-set)))]
      )))

(check-expect (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))

(define a-pair?
  (λ (x)
    (cond
      [(null? x) #f]
      [(null? (car x)) #f]
      [(null? (cdr x)) #f]
      [(null? (car (cdr x))) #f]
      [else (null? (cdr (cdr x)))]
      )))

(check-expect (a-pair? '(3 7)) #t)
(check-expect (a-pair? '(full (house))) #t)
(check-expect (a-pair? '(3 7 9)) #f)

(define first (λ (p) (car p)))
(define second (λ (p) (car (cdr p))))
(define build (λ (s1 s2) (cons s1 (cons s2 '()))))


(define fun? (λ (rel) (set? (firsts rel))))

(check-expect (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #t)

(define revrel
  (λ (rel)
    (cond
      [(null? rel) '()]
      [else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))]
      )))

(check-expect (revrel '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))

(define fullfun? (λ (rel) (fun? (revrel rel))))

(test)
