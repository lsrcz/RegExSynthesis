#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/angelic
         rosette/lib/synthax)

(require "match.rkt"
         "model.rkt")

(define (solve-match r l starnum)
  (define model (synthesize #:forall (list) #:guarantee (assert (car (regex-match-wrapper r l starnum)))))
  (if (unsat? model)
      #f
      model))

(define (eqsym a b)
    (match a
      [(constant identifier type)
       (match b
         [(constant i2 t2) (eq? identifier i2)])]))

(define (membersym a lst)
  (if (null? lst)
      #f
      (if (eqsym a (car lst))
          #t
          (membersym a (cdr lst)))))

(define (solve-nmatch r l starnum)
  (define oriset (symbolics r))
  (define ret (regex-match-wrapper r l starnum))
  (define newset (filter (lambda (x) (not (membersym x oriset))) (symbolics (cdr ret))))
  (define model
    (synthesize #:forall newset
                #:guarantee (assert (not (car ret)))))
  (if (unsat? model)
      #f
      model))

(define (solve-match-lst r gl bl starnum)
  (define (glclause gl)
    (if (null? gl)
        #t
        (and (car (regex-match-wrapper r (car gl) starnum)) (glclause (cdr gl)))))
  
  (define (blclause bl)
    (define oriset (symbolics r))
    (if (null? bl)
        (cons #t '())
        (begin
          (define ret (regex-match-wrapper r (car bl) starnum))
          (define newset (filter (lambda (x) (not (membersym x oriset))) (symbolics (cdr ret))))
          (define nxt (blclause (cdr bl)))
          (cons (and (not (car ret)) (car nxt)) (append newset (cdr nxt))))))
  (define gc (glclause gl))
  (define bcp (blclause bl))
  (define model (synthesize #:forall (cdr bcp) #:guarantee (assert (and gc (car bcp)))))
  (if (unsat? model)
      #f
      model))

(define (test-synth)
  (define sereg (choose* (single 2) (single 3))); (star (select (single 2) (single 3)))))
  (print sereg)
  (print (solve-match sereg '(2) 2))
  (print (solve-match sereg '(3) 2))
  (print (evaluate sereg (solve-nmatch sereg '(2) 2))))