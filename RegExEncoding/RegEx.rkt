#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/angelic
         rosette/lib/synthax)

(struct concat (left right) #:transparent)
(struct select (if-true if-false) #:transparent)
(struct star (base) #:transparent)
(struct single (sym) #:transparent)
(struct eps () #:transparent)
(struct fromto (from to) #:transparent)

(define (print-regex p)
  (match p
    [(single sym) (display sym)]
    [(select a b) (begin
                    (display "(")
                    (print-regex a)
                    (display "|")
                    (print-regex b)
                    (display ")"))]
    [(concat a b) (begin (print-regex a) (print-regex b))]
    [(star a) (begin
                (display "(")
                (print-regex a)
                (display ")*"))]
    [(eps) (display "ε")]
    [(fromto from to) (begin
                        (display "[")
                        (display from)
                        (display "-")
                        (display to)
                        (display "]"))]))

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


(regex-match-wrapper (concat (single 2) (concat (fromto 0 4) (fromto 0 9))) '(2 5 6) 0)
(regex-match-wrapper (concat (single 2) (concat (fromto 0 4) (fromto 0 9))) '(2 4 6) 0)
(regex-match-wrapper (concat (single 2) (concat (single 5) (fromto 0 5))) '(2 5 6) 0)
(regex-match-wrapper (concat (fromto 0 1) (concat (fromto 0 9) (fromto 0 9))) '(2 5 6) 0)
(define t (regex-match-wrapper (star (select (single 2) (single 3))) '(2) 2))
(car t)
(symbolics (cdr t))
(newline)

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

(define sereg (choose* (single 2) (single 3))); (star (select (single 2) (single 3)))))
sereg
(solve-match sereg '(2) 2)
(solve-match sereg '(3) 2)
(evaluate sereg (solve-nmatch sereg '(2) 2))

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

(define sk2 (choose* (fromto 0 9) (single 10)))
(define m2 (solve-match sk2 (list 1) 3))
(print-regex (evaluate sk2 m2))
(newline)

(define (??num)
  (define (singlehole)
    (apply choose* '(0 1 2 3 4 5 6 7 8 9)))
  (choose*
   (single (singlehole))
   (fromto (singlehole) (singlehole))))
(define (??h1)
  (concat (??num) (concat (??num) (??num))))
(??num)

(define x (??num))

(evaluate x (complete-solution (solve-match-lst x '((4) (5)) #| (list (list 0) (list 1)) |# '((0) (3) (6)) 0) (symbolics x)))

(define ipsk (select (select (??h1) (??h1)) (??h1)))

(define ip1
  (select
   (select (concat (single 2) (concat (fromto 0 4) (fromto 0 9)))
           (concat (single 2) (concat (single 5) (fromto 0 5))))
   (concat (fromto 0 1) (concat (fromto 0 9) (fromto 0 9)))))

(solve-match-lst ip1
                 (list '(1 2 3)
                       '(0 9 0)
                       '(2 5 5)
                       '(0 1 2)
                       '(0 9 1))
                 (list '(2 5 6)
                       '(3 2 1))
                 0)

(define ipmodel (time (complete-solution
                       (solve-match-lst ipsk
                                        (list '(0 0 0)
                                              '(0 9 9)
                                              '(1 0 0)
                                              '(1 9 9)
                                              '(2 0 0)
                                              '(2 4 9)
                                              '(2 5 0)
                                              '(2 5 5))
                                        (list '(2 5 6)
                                              '(2 6 0)
                                              '(2 6 9)
                                              '(2 9 9)
                                              '(3 0 0)
                                              '(9 9 9))
                                        0)
                       (symbolics ipsk))))

(solve-match-lst (select
                  (select (concat (fromto 1 2) (concat (fromto 0 9) (fromto 0 9)))
                          (concat (single 0) (concat (fromto 0 9) (fromto 0 9))))
                  (concat (fromto 2 2) (concat (fromto 3 7) (fromto 3 9))))
                 (list)
                                        (list '(2 5 6)
                                              '(2 6 5)
                                              '(3 2 1)
                                              '(4 0 0))
                                        0)

(print-regex (evaluate ipsk ipmodel))
  
#|
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

(solve-match reg '(a b a b b a))|#

         
           