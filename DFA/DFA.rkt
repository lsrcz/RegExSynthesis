#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match)

(struct dfanode (acc nlst) #:transparent)

(struct dfa (lst) #:transparent)

(define (dfa-match d l)
  (define (match-iter nowidx l)
    (if (= -1 nowidx)
        #f
        (match d
          [(dfa lst)
           (let ([now (list-ref lst nowidx)])
             (if (null? l)
                 (dfanode-acc now)
                 (let ([v (assoc (car l) (dfanode-nlst now))])
                   (if v
                       (match-iter (cdr v) (cdr l))
                       #f))))])))
  (match-iter 0 l))