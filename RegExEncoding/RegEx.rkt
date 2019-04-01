#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match)

(struct concat (left right) #:transparent)
(struct select (if-true if-false if-cond) #:transparent)
(struct star (base num) #:transparent)
(struct single (sym) #:transparent)

(define (star-match base num l)
  (if (= num 0)
      0
      (let* ([ret (regex-match base l)]
             [back (if ret (star-match base (- num 1) (drop l ret)) #f)])
        (if (and ret back)
            (+ ret back)
            #f))))

(define (regex-match r l)
  (match r
    [(single sym)
     (if (null? l)
         #f
         (if (eq? (car l) sym)
             1
             #f))]
    [(concat left right)
     (let* ([lm (regex-match left l)]
            [rm (if lm (regex-match right (drop l lm)) #f)])
       (if (and lm rm)
           (+ lm rm)
           #f))]
    [(select if-true if-false if-cond)
     (if if-cond
         (regex-match if-true l)
         (regex-match if-false l))]
    [(star base num)
     (star-match base num l)]
    ))

(define (get-concat left right)
  (concat left right))
(define (get-select if-true if-false)
  (define-symbolic* if-cond boolean?)
  (select if-true if-false if-cond))
(define (get-star base)
  (define-symbolic* num integer?)
  (star base num))
(define (get-single sym)
  (single sym))

(define (solve-match r l)
  (define model (synthesize #:forall (list) #:guarantee (assert (= (regex-match r l) (length l)))))
  (if (unsat? model)
      #f
      (evaluate r model)))

(regex-match (star (select (concat (single 'a) (single 'b)) (concat (single 'b) (single 'a)) #t) 3) '(a b a b))

(define reg (get-star (get-select (get-concat (get-single 'a) (get-single 'b)) (get-concat (single 'b) (single 'a)))))

(solve-match reg '(a b a b b a))


         
           