#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/angelic
         rosette/lib/synthax)

(require racket/set)

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
  #|(define oriset (symbolics r))
  (define (blclause bl)
    (if (null? bl)
        (cons #t '())
        (begin
          (define ret (regex-match-wrapper r (car bl) starnum))
          (define newset (time (filter (lambda (x) (not (membersym x oriset))) (symbolics (cdr ret)))))
          (define nxt (blclause (cdr bl)))
          (cons (and (not (car ret)) (car nxt)) (append newset (cdr nxt))))))|#
  (define oriset (list->set (symbolics r)))
  (define (blclause bl)
    (if (null? bl)
        (cons #t '())
        (begin
          (define ret (regex-match-wrapper r (car bl) starnum))
          (define newset (filter (lambda (x) (not (set-member? oriset x))) (symbolics (cdr ret))))
          (define nxt (blclause (cdr bl)))
          (cons (and (not (car ret)) (car nxt)) (append newset (cdr nxt))))))
  (displayln "Build constraints for good strings")
  (define gc (time (glclause gl)))
  (displayln "Build constraints for bad strings")
  (define bcp (time (blclause bl)))
  (displayln "Total symbolic values")
  (displayln (length (symbolics (and gc (car bcp)))))
  (displayln "Synthesis")
  (define model (time (synthesize #:forall (cdr bcp) #:guarantee (assert (and gc (car bcp))))))
  (if (unsat? model)
      #f
      model))

(define (test-synth)
  (define sereg (choose* (single 2) (single 3))); (star (select (single 2) (single 3)))))
  (print sereg)
  (print (solve-match sereg '(2) 2))
  (print (solve-match sereg '(3) 2))
  (print (evaluate sereg (solve-nmatch sereg '(2) 2)))
  (define sk2 (choose* (fromto 0 9) (single 10)))
  (define m2 (solve-match sk2 (list 1) 3))
  (print-regex (evaluate sk2 m2))
  (newline))