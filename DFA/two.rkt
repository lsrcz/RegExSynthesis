#lang rosette/safe

(current-bitwidth 5)

(require rosette/lib/match
         rosette/lib/synthax
         rosette/lib/angelic)

(require "DFA.rkt"
         "sketch.rkt"
         "oracle.rkt")

(define (println x) (begin (print x) (newline)))

(define (synthesize-two-dfa sk1 sk2 charset goodlst badlst maxstrlen additional-constraint)
  (define strskc (??string* charset maxstrlen))
  (define (goodclause goodlst sk)
    (if (null? goodlst)
        #t
        (and (dfa-match sk (car goodlst)) (goodclause (cdr goodlst) sk))))
  (define (badclause badlst sk)
    (if (null? badlst)
        #t
        (and (not (dfa-match sk (car badlst))) (badclause (cdr badlst) sk))))
  (define model1 (synthesize #:forall (symbolics strskc)
                             #:guarantee (assert (and (goodclause goodlst sk1)
                                                      (badclause badlst sk1)
                                                      (additional-constraint sk1 strskc)))))

  (if (unsat? model1)
      #f
      (letrec ([fullmodel1 (complete-solution model1 (symbolics sk1))]
               [strsk (??string* charset maxstrlen)]
               [diffclause (lambda (lst)
                             (if (null? lst)
                                 #t
                                 (and (not (eq? strsk (car lst))) (diffclause (cdr lst)))))]
             [model2 (synthesize #:forall (symbolics strskc)
                                 #:guarantee (assert (and (goodclause goodlst sk2)
                                                          (badclause badlst sk2)
                                                          (diffclause goodlst)
                                                          (diffclause badlst)
                                                          (additional-constraint sk2 strskc)
                                                          (xor (dfa-match (evaluate sk1 fullmodel1) strsk)
                                                               (dfa-match sk2 strsk)))))])
        (if (unsat? model2)
            (list (evaluate sk1 fullmodel1) #f #f)
            (let ([fullmodel2 (complete-solution model2 (append (symbolics strsk) (symbolics sk2)))])
              (list (evaluate sk1 fullmodel1) (evaluate sk2 fullmodel2) (evaluate strsk fullmodel2)))))))

(define (interactive-two-loop charset numofstate maxstrlen initgood initbad
                              [readfunc (lambda (str)
                                          (begin (display "Is the string acceptible? (#t/#f)")
                                                 (display str) (read)))]
                              [additional-constraint (lambda (sk strskc) #t)])
  (define sk1 (??sketch* numofstate charset))
  (define sk2 (??sketch* numofstate charset))
  (define (iter goodlst badlst num)
    (let ([ret (synthesize-two-dfa sk1 sk2 charset goodlst badlst maxstrlen additional-constraint)])
      (if ret
          (if (cadr ret)
              ; has distinguishing string
              (let* ([str (caddr ret)]
                     [ret (readfunc str)])
                (if ret
                    (iter (cons str goodlst) badlst (+ 1 num))
                    (iter goodlst (cons str badlst) (+ 1 num))))
              (begin
                (display "Success in ")
                (display num)
                (display " iters.")
                (newline)
                (car ret)))
          (unsat))))
  (iter initgood initbad 1))

(define (oracle-read-func oracle)
  (lambda (str) (let ([ret (dfa-match oracle str)]) (begin (display str) (display ": ") (displayln ret) ret))))



(define (main)
  ; (a|b)*abb
  ; 18 iters, 512 tot, 37s
  #;(time (println (interactive-two-loop '(a b) 4 8
                                 '((a a b b) (b a b b) (a b a b b) (a b b))
                                 '((a b) (a a a b) (b b a b))
                                 (oracle-read-func oracle1)
                                 )))
  ; 35 iters, 512 tot, 82s
  #;(time (println (interactive-two-loop '(a b) 4 8 '() '() (oracle-read-func oracle1))))
  ; 29 iters, 128 tot, 70s
  #;(time (println (interactive-two-loop '(a b) 4 6 '() '() (oracle-read-func oracle1))))
  ; 25 iters, 2048 tot, 73s
  #;(time (println (interactive-two-loop '(a b) 4 10 '() '() (oracle-read-func oracle1))))
  ; (ab|cd)*
  ; very long time
  ; (println (interactive-two-loop '(a b c d) 5 8 '() '()))
  ; timeout
  (time (println (interactive-two-loop
                  '(a b c d) 5 6
                  '((a b) (c d) (a b c d) (c d a b) (a b a b) (c d c d) (a b a b a b) (c d c d c d))
                  '()
                  (oracle-read-func oracle2)

                  )))
  )