#lang rosette/safe

(provide (all-defined-out))

(require "RegEx.rkt"
         "NFAOptimize.rkt"
         "sketch.rkt")

(require rosette/lib/match
         rosette/lib/synthax
         rosette/lib/angelic)

(define (epsfree-nfa-match n l)
  (define (is-acc lst)
    (not (andmap (lambda (idx) (null? (nnode-eps (get-node-by-idx n idx)))) lst)))
  (define (match-iter nowlst l)
    (if (null? nowlst)
        #f
        (if (null? l)
            (is-acc nowlst)
            (match n
              [(nfa _ _ lst)
               (let ([new
                      (mergesort
                       (foldl
                        (lambda (x acc)
                          (let* ([node (get-node-by-idx n x)]
                                 [nxt (assoc (car l) (nnode-nxt node))])
                            (if nxt
                                (append (cdr nxt) acc)
                                acc)))
                        '()
                        nowlst))])
                 (match-iter new (cdr l)))]))))
  (match-iter (list (nfa-start n)) l))