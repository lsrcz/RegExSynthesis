#lang rosette/safe

(require "matching.rkt"
         "RegEx.rkt"
         "sketch.rkt"
         "NFAOptimize.rkt")

(define sk1 (??regex '(1 2) 2))

(length (symbolics sk1))

(define M1 (synthesize #:forall (list)
                      #:guarantee (assert (epsfree-nfa-match (remove-eps (build sk1)) '(1 1)))))
M1
(evaluate sk1 (complete-solution M1 (symbolics sk1)))

(define sk3 (??regex '(1 2) 3))

(length (symbolics sk3))

(define M3 (synthesize #:forall (list)
                      #:guarantee (assert (epsfree-nfa-match (remove-eps (build sk3)) '(1 1)))))
M3
(evaluate sk3 (complete-solution M3 (symbolics sk1)))


(define sk2 (regex 1 2 1))

(define M2 (synthesize #:forall (list)
                      #:guarantee (assert (epsfree-nfa-match (remove-eps (build sk2)) '(1 1)))))
M2

(evaluate sk2 (complete-solution M2 (symbolics sk2)))
