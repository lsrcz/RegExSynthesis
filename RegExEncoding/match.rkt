#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/angelic
         rosette/lib/synthax)

(require "model.rkt")

(define (regex-match-wrapper r l starnum)
(define (regex-match r l starnum)
  (define (changestar a starnum)
    (if (= 0 starnum)
        (cons (eps) '())
        (begin
          (define-symbolic* st boolean?)
          (let ([ret (changestar a (- starnum 1))])
            (cons (if st
                      (eps)
                      (concat a (car ret)))
                  (cons st (cdr ret)))))))
  (match r
    [(single sym)
     (if (null? l)
         (cons #f '())
         (if (= (car l) sym)
             (cons 1 '())
             (cons #f '())))]
    [(concat left right)
     (let* ([lm (regex-match left l starnum)]
            [rm (if (car lm) (regex-match right (drop l (car lm)) starnum) (cons #f (cdr lm)))])
       (if (and (car lm) (car rm))
           (cons (+ (car lm) (car rm)) (append (cdr lm) (cdr rm)))
           (cons #f (append (cdr lm) (cdr rm)))))]
    [(select if-true if-false)
     (begin
       (define-symbolic* se boolean?)
       (let ([ret (if se
                      (regex-match if-true l starnum)
                      (regex-match if-false l starnum))])
         (cons (car ret) (cons se (cdr ret)))))]
    [(star base)
     (let* ([ret1 (changestar base starnum)]
            [ret2 (regex-match (car ret1) l starnum)])
       (cons (car ret2) (append (cdr ret1) (cdr ret2))))]
    [(fromto from to)
     (if (and (>= (car l) from) (<= (car l) to))
         (cons 1 '())
         (cons #f '()))]
    [(eps) (cons 0 '())]
    ))
  (let ([ret (regex-match r l starnum)])
    (if (car ret)
        (cons (= (car ret) (length l)) (cdr ret))
        ret)))

(define (test-match)
  (print (regex-match-wrapper (concat (single 2) (concat (fromto 0 4) (fromto 0 9))) '(2 5 6) 0))
  (print (regex-match-wrapper (concat (single 2) (concat (fromto 0 4) (fromto 0 9))) '(2 4 6) 0))
  (print (regex-match-wrapper (concat (single 2) (concat (single 5) (fromto 0 5))) '(2 5 6) 0))
  (print (regex-match-wrapper (concat (fromto 0 1) (concat (fromto 0 9) (fromto 0 9))) '(2 5 6) 0))
  (define t (regex-match-wrapper (star (select (single 2) (single 3))) '(2) 2))
  (print (car t))
  (print (symbolics (cdr t)))
  (newline))