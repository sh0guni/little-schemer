#lang racket
(require test-engine/racket-tests)
(require "little-schemer.rkt")

(define numbered?
  (λ (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp)))))]
      )))

(check-expect (numbered? '(3 + (4 × 5))) #t)

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
      [(eq? (operator nexp) '+)
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) '×)
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))]
      [else
       (expt
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))]
      )))

(check-expect (value 13) 13)
(check-expect (value '(+ 1 3)) 4)
(check-expect (value '(+ 1 (↑ 3 4))) 82)

(test)
