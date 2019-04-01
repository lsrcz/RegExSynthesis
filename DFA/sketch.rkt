#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/angelic
         rosette/lib/match)

(require "DFA.rkt")

(define (??sketch* statenum alphalist)
  (define (??nxt* alpha statelist)
    (cons alpha (apply choose* statelist)))
  (define (??node* statelist alphalist)
    (dfanode (choose* #f #t) (map (lambda (alpha) (??nxt* alpha statelist)) alphalist)))
  (define (get-statelist x)
    (if (= x statenum)
        '()
        (cons x (get-statelist (+ 1 x)))))
  (define statelist (get-statelist -1))
  (dfa (map (lambda (state) (??node* statelist alphalist)) (cdr statelist))))

(define (??string* charset len)
  (if (= len 0)
      '()
      (choose* '() (cons (apply choose* charset) (??string* charset (- len 1))))))
      ;(cons (apply choose* charset) (choose* (??string* charset (- len 1)) '()))))
