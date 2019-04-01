#lang rosette/safe

(require rosette/lib/angelic
         rosette/lib/synthax
         rosette/lib/match)

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

(define d
  [dfa (list [dfanode #t (list (cons 3 0) (cons 4 0))]
             [dfanode #t (list)])])

;(dfa-match d '(3 4))





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

(define sk (??sketch* 2 '(a b)))

#|
;(symbolics sk)
;(print d)
;(newline)
;(print sk)

(define M
  (time (synthesize
   #:forall (list)
   #:guarantee (assert (not (dfa-match sk '(a b)))))))
                            ;(not (dfa-match sk '(b a))))))))

;(print M)
;(evaluate sk (complete-solution M (symbolics sk)))|#

(define (synthesize-dfa-from-sketch sk charset goodlst badlst)
  (define (goodclause goodlst)
    (if (null? goodlst)
        #t
        (and (dfa-match sk (car goodlst)) (goodclause (cdr goodlst)))))
  (define (badclause badlst)
    (if (null? badlst)
        #t
        (and (not (dfa-match sk (car badlst))) (badclause (cdr badlst)))))
  (define model (synthesize #:forall (list)
                            #:guarantee (assert (and (goodclause goodlst) (badclause badlst)))))
  (complete-solution model (symbolics sk)))

(define (get-dfa numofstate charset goodlst badlst)
  (let ([sk (??sketch* numofstate charset)])
    (cons sk
          (synthesize-dfa-from-sketch sk charset goodlst badlst))))
#|
    (let ([csolver (current-solver)])
      (begin
        (solver-push csolver)
        (solver-assert csolver (list (goodclause goodlst) (badclause badlst)))
        (let ([model (solver-check csolver)])
          (begin (solver-pop csolver)
                 (if (sat? model)
                     (let ([evaluated (evaluate sk (complete-solution model (symbolics sk)))])
                       (begin
                         (displayln (evaluate (and (goodclause goodlst) (badclause badlst)) (complete-solution model (symbolics sk))))
                         evaluated))
                     #f)))))))|#

(define (??string* charset len)
  (if (= len 0)
      '()
      (cons (apply choose* charset) (choose* (??string* charset (- len 1)) '()))))

(define t (??string* '(a b) 2))
(define M (synthesize #:forall '()
                      #:guarantee (assert (equal? t '(a b)))))
(evaluate t M)

(define (get-good-str charset goodlst sk model maxstrlen)
  (define strsk (??string* charset maxstrlen))
  (define (clause goodlst)
    (if (null? goodlst)
        #t
        (and (not (eq? strsk (car goodlst))) (clause (cdr goodlst)))))
  (define strmodel (synthesize #:forall (list)
                               #:guarantee (assert (and (clause goodlst) (dfa-match (evaluate sk model) strsk) ))))
  (if (unsat? strmodel)
      #f
      (evaluate strsk (complete-solution strmodel (symbolics strsk)))))

(define (get-bad-str charset badlst sk model maxstrlen)
  (define strsk (??string* charset maxstrlen))
  (define (clause badlst)
    (if (null? badlst)
        #t
        (and (not (eq? strsk (car badlst))) (clause (cdr badlst)))))
  (define strmodel (synthesize #:forall (list)
                               #:guarantee (assert (and (clause badlst) (not (dfa-match (evaluate sk model) strsk))))))
  (if (unsat? strmodel)
      #f
      (evaluate strsk (complete-solution strmodel (symbolics strsk)))))

(define (get-nxt-interactively charset sk model maxstrlen isgood lst num)
  (define (ask-for-opinion str)
    (display "Is the string acceptible")
    (display (if isgood "(assume yes)" "(assume no)"))
    (display "(#t/#f): ")
    (displayln str)
    (read))
  (if (= num 0)
      (cons lst #f)
      (let* ([str ((if isgood get-good-str get-bad-str) charset lst sk model maxstrlen)]
             [opinion (if str (ask-for-opinion str) #f)])
        (if str
            (if (eq? opinion isgood)
                (get-nxt-interactively charset sk model maxstrlen isgood (cons str lst) (- num 1))
                (cons lst str))
            (cons lst #f)))))
         

(define (interactive-loop charset numofstate maxstrlen initgood initbad)
  (define sk (??sketch* numofstate charset))
  (define (iter goodlst badlst)
    (let ([ret (synthesize-dfa-from-sketch sk charset goodlst badlst)])
      (if (unsat? ret)
          (displayln "Unsat")
          (begin
            (displayln "Now dfa is:")
            (print (evaluate sk ret))
            (newline)
            (let ([goodret (get-nxt-interactively charset sk ret maxstrlen #t goodlst 5)])
              (begin (newline) (print goodret) (newline)
              (if (cdr goodret)
                  (iter (car goodret) (cons (cdr goodret) badlst))
                  (let ([badret (get-nxt-interactively charset sk ret maxstrlen #f badlst 5)])
                    (if (cdr badret)
                        (iter (cons (cdr badret) (car goodret)) (car badret))
                        (begin
                          (displayln "Cannot find a wrong string in 5 iters, stop?")
                          (if (read)
                              (evaluate sk ret)
                              (iter (car goodret) (car badret)))))))))))))
  (iter initgood initbad))

;(interactive-loop '(a b) 2 6 '((a b) (a b a b)) '((b a) (a)))
;(interactive-loop '(a b) 4 8 '((a a b b) (b a b b) (a b a b b) (a b b)) '((a b) (a a a b) (b b a b)))

(define (synthesize-two-dfa sk1 sk2 charset goodlst badlst maxstrlen)
  (define (goodclause goodlst sk)
    (if (null? goodlst)
        #t
        (and (dfa-match sk (car goodlst)) (goodclause (cdr goodlst)))))
  (define (badclause badlst sk)
    (if (null? badlst)
        #t
        (and (not (dfa-match sk (car badlst))) (badclause (cdr badlst)))))
  (define model1 (synthesize #:forall (list)
                             #:guarantee (assert (and (goodclause goodlst sk1) (badclause badlst sk1)))))

  (if (unsat? model1)
      #f
      (letrec ([fullmodel1 (complete-solution model1 (symbolics sk1))]
               [strsk (??string* charset maxstrlen)]
               [diffclause (lambda (lst)
                             (if (null? lst)
                                 #t
                                 (and (not (eq? strsk (car lst))) (diffclause (cdr lst)))))]
             [model2 (synthesize #:forall (list)
                                 #:guarantee (assert (and (goodclause goodlst sk2)
                                                          (badclause badlst sk2)
                                                          (diffclause goodlst)
                                                          (diffclause badlst)
                                                          (xor (dfa-match (evaluate sk1 fullmodel1) strsk)
                                                               (dfa-match sk2 strsk)))))])
        (if (unsat? model2)
            (list (evaluate sk1 fullmodel1) #f #f)
            (let ([fullmodel2 (complete-solution model2 (append (symbolics strsk) (symbolics sk2)))])
              (list (evaluate sk1 fullmodel1) (evaluate sk2 fullmodel2) (evaluate strsk fullmodel2)))))))

(synthesize-two-dfa (??sketch* 2 '(a b)) (??sketch* 2 '(a b)) '(a b) '() '() 6)

#;(time (let ([ret (get-dfa 2 '(a b) '((a b)) '((b a)))])
        (evaluate (car ret) (cdr ret))))
#|(time (get-dfa 5 '(a b c d)
               '((a b a b) (a b c d) (c d a b) (c d c d) (a b) (c d) (a b a b a b))
               '((a b c) (a b a) (a b d) (b a d) (a c d) (a c) (b d) (a d) (b c))))|#

