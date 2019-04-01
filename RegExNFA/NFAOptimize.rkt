#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/synthax
         rosette/lib/angelic)

(require "RegEx.rkt")

(define (mergesort lst [leqfunc <] [eqfunc =])
  (remove-duplicates (sort lst leqfunc) eqfunc))


; obsolete, performance issue, only for reference
(define (set-nfa-closure-ref n)
  (define (update idx-lst lst)
    (mergesort
     (foldr (lambda (x acc)
              (if (member (nnode-idx x) idx-lst)
                  (append (nnode-eps x) acc)
                  acc))
            '() lst)))
  (define (iter n)
    (let* ([lst (map
                 (lambda (x)
                   (match x
                     [(nnode idx nxt eps)
                      (nnode idx nxt (if (member idx eps) eps (cons idx eps)))]))
                 (nfa-lst n))]
           [ret (foldr (lambda (x acc)
                         (let ([neweps (update (nnode-eps x) lst)])
                           (cons
                            (cons (nnode (nnode-idx x)
                                         (nnode-nxt x)
                                         neweps)
                                  (if (null? acc) '() (car acc)))
                            (if (null? acc)
                                (not (eq? neweps (nnode-eps x)))
                                (or (cdr acc) (not (eq? neweps (nnode-eps x))))))))
                       '() lst)])
      (cons (nfa (nfa-start n)
                 (nfa-end n)
                 (car ret)) (cdr ret))))
  (let ([ret (iter n)])
    (if (cdr ret) (set-nfa-closure-ref (car ret))
        (car ret))))

(define (get-node-by-idx n idx)
  (define (iter lst)
    (if (null? lst)
        #f
        (match (car lst)
          [(nnode i _ _)
           (if (= i idx)
               (car lst)
               (iter (cdr lst)))])))
  (iter (nfa-lst n)))

(define (remove-eps n)
  (define (init-iter lst)
    (if (null? lst) '()
        (foldl (lambda (x acc) (append (cdr x) acc)) (init-iter (cdr lst)) (nnode-nxt (car lst)))))
  (define init (remove-duplicates (sort (cons (nfa-start n) (init-iter (nfa-lst n))) <)))
  (define (merge-same lst)
    (define (iter lst all)
      (if (null? lst)
          all
          (if (null? all)
              (iter (cdr lst) (list (car lst)))
              (if (eq? (caar lst) (caar all))
                  (iter (cdr lst) (cons (cons (caar all) (append (cdar lst) (cdar all))) (cdr all)))
                  (iter (cdr lst) (cons (car lst) all))))))
    (iter lst '()))
  (define (get-newnode num)
    (define (iter stack visited lsts end)
      (if (null? stack)
          (cons lsts end)
          (if (member (car stack) visited)
              (iter (cdr stack) visited lsts end)
              (let ([node (get-node-by-idx n (car stack))]
                    [newend (or end (= (car stack) (nfa-end n)))])
                (iter (append (nnode-eps node) (cdr stack))
                      (cons (car stack) visited)
                      (append (nnode-nxt node) lsts) newend)))))
    (let ([ret (iter (list num) '() '() #f)])
      (nnode num (map (lambda (x) (cons (car x) (remove-duplicates (sort (cdr x) <) =)))
                      (merge-same (sort (car ret) < #:key car))) (if (cdr ret) (list (nfa-end n)) '()))))
  (nfa (nfa-start n) (nfa-end n) (map (lambda (x) (get-newnode x)) init)))

