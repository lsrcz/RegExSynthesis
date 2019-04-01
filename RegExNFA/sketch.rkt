#lang rosette/safe

(provide (all-defined-out))

(require "RegEx.rkt")

(require rosette/lib/match
         rosette/lib/synthax
         rosette/lib/angelic)


(define (??regex charset maxsize)
  (define (get-list n)
    (define (iter n)
      (if (= n 0)
          '()
          (cons n (iter (- n 1)))))
    (iter n))
  (define (??nonconcat maxsize)
    (if (= maxsize 1)
        (apply choose* charset)
        (if (= maxsize 2)
            (choose* (apply choose* charset)
                     (starinst (??regex charset (- maxsize 1))))
            (choose* (apply choose* charset)
                     (starinst (??regex charset (- maxsize 1)))
                     (apply choose* (map (lambda (x) (orinst (??regex charset x) (??regex charset (- (- maxsize 1) x)))) (get-list (- maxsize 2))))))))
  (if (= maxsize 1)
      (apply choose* charset)
      (if (= maxsize 2)
          (choose* (??nonconcat maxsize))
          (choose* (??nonconcat maxsize)
                   (apply choose* (map (lambda (x) (concatinst (??nonconcat x) (??nonconcat (- (- maxsize 1) x)))) (get-list (- maxsize 2))))))))

(define-synthax (regex x y depth)
  #:base (choose x y)
  #:else (choose
          x y
          (starinst (regex x y (- depth 1)))
          (concatinst (regex x y (- depth 1)) (regex x y (- depth 1)))
          (orinst (regex x y (- depth 1)) (regex x y (- depth 1)))))

(define (main)
  (print (regex 1 2 1))
  (print (??regex '(1 2) 3)))