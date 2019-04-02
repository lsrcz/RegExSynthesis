#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/angelic
         rosette/lib/synthax)

(require "match.rkt"
         "model.rkt")


(define (??regex charset maxsize)
  (define (get-list n)
    (define (iter n)
      (if (= n 0)
          '()
          (cons n (iter (- n 1)))))
    (iter n))
  (define (??nonconcat maxsize)
    (if (= maxsize 1)
        (single (apply choose* charset))
        (if (= maxsize 2)
            (choose* (single (apply choose* charset))
                     (star (??regex charset (- maxsize 1))))
            (choose* (single (apply choose* charset))
                     (star (??regex charset (- maxsize 1)))
                     (apply choose* (map (lambda (x) (select (??regex charset x) (??regex charset (- (- maxsize 1) x)))) (get-list (- maxsize 2))))))))
  (if (= maxsize 1)
      (single (apply choose* charset))
      (if (= maxsize 2)
          (choose* (??nonconcat maxsize))
          (choose* (??nonconcat maxsize)
                   (apply choose* (map (lambda (x) (concat (??nonconcat x) (??nonconcat (- (- maxsize 1) x)))) (get-list (- maxsize 2))))))))

(define (??num)
  (define (singlehole)
    (apply choose* '(0 1 2 3 4 5 6 7 8 9)))
  (choose*
   (single (singlehole))
   (fromto (singlehole) (singlehole))))