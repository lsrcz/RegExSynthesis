#lang rosette/safe

(require rosette/lib/match
         rosette/lib/synthax
         rosette/lib/angelic)

(require "DFA.rkt"
         "sketch.rkt"
         "oracle.rkt")

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

(define (get-nxt-interactively charset sk model maxstrlen isgood lst num readfunc)
  (define (ask-for-opinion str)
    (display "Is the string acceptable")
    (display (if isgood "(assume yes)" "(assume no)"))
    (display "(#t/#f): ")
    (displayln str)
    (readfunc str))
  (if (= num 0)
      (cons lst #f)
      (let* ([str ((if isgood get-good-str get-bad-str) charset lst sk model maxstrlen)]
             [opinion (if str (ask-for-opinion str) #f)])
        (if str
            (if (eq? opinion isgood)
                (get-nxt-interactively charset sk model maxstrlen isgood (cons str lst) (- num 1) readfunc)
                (cons lst str))
            (cons lst #f)))))

(define (interactive-loop charset numofstate maxstrlen initgood initbad
                          [readfunc (lambda (str)
                                          (begin (display "Is the string acceptible? (#t/#f)")
                                                 (display str) (read)))])
  (define sk (??sketch* numofstate charset))
  (define (iter goodlst badlst cnt)
    (let ([ret (synthesize-dfa-from-sketch sk charset goodlst badlst)])
      (if (unsat? ret)
          (displayln "Unsat")
          (begin
            (displayln "Now dfa is:")
            (print (evaluate sk ret))
            (newline)
            (let ([goodret (get-nxt-interactively charset sk ret maxstrlen #t goodlst 5 readfunc)])
              (if (cdr goodret)
                  (iter (car goodret) (cons (cdr goodret) badlst) 0)
                  (let ([badret (get-nxt-interactively charset sk ret maxstrlen #f badlst 5 readfunc)])
                    (if (cdr badret)
                        (iter (cons (cdr badret) (car goodret)) (car badret) 0)
                        (begin
                          (displayln "Cannot find a wrong string in 5 iters, stop?")
                          (if (= cnt 2);(read)
                              (evaluate sk ret)
                              (iter (car goodret) (car badret) (+ 1 cnt))))))))))))
  (iter initgood initbad 0))

(define (main)
  (begin
    ;(interactive-loop '(a b) 4 4 '((a a b b) (b a b b) (a b a b b) (a b b)) '((a b) (a a a b) (b b a b)) (oracle-read-func oracle1))
    (time (interactive-loop '(a b) 4 8
                            '((a a b b) (b a b b) (a b a b b) (a b b))
                            '((a b) (a a a b) (b b a b))
                            (oracle-read-func oracle1)
                            ))
    (time (interactive-loop
                  '(a b c d) 3 6
                  '()
                  '()
                  (oracle-read-func oracle2)
                  ))
    (time (interactive-loop
                  '(a b c d) 3 6
                  '((a b) (c d) (a b c d) (c d a b) (a b a b) (c d c d) (a b a b a b) (c d c d c d))
                  '()
                  (oracle-read-func oracle2)
                  ))
    ))
