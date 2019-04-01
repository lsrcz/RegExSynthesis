#lang rosette

(require "matching.rkt"
         "RegEx.rkt"
         "sketch.rkt"
         "NFAOptimize.rkt")

(define (print-nfa n)
  (define (get-sym-lst lst)
    (define (iter l)
      (if (null? l) '()
          (append (map car (nnode-nxt (car l))) (iter (cdr l)))))
    (remove-duplicates (sort (iter lst) <) =))
  (match n
    [(nfa start end lst)
     (let ([sym-lst (get-sym-lst lst)])
       (for-each (lambda (x)
                   (match x
                     [(nnode idx nxt eps)
                      (begin (display idx)
                             (if (= idx start)
                                 (display "s")
                                 (void))
                             (if (= idx end)
                                 (display "*")
                                 (void))
                             (display "\t")
                             (for-each (lambda (x)
                                         (let ([v (assoc x nxt)])
                                           (if v
                                               (begin
                                                 (display v) (display "\t"))
                                               (void)))) sym-lst)
                             (if (null? eps)
                                 (void)
                                 (display (cons "eps" eps)))
                             (display "\n"))])) lst))]))


(define epsn  (remove-eps (build (orinst (orinst 2 2) (orinst 1 2)))))

(epsfree-nfa-match epsn (list 3))

(newline)

(define nfa1 (build (starinst 1)))

(displayln nfa1)
(newline)
(print-nfa nfa1)
(newline)
(print-nfa (set-nfa-closure-ref nfa1))
(newline)
(print-nfa (remove-eps nfa1))

(define nfa2 (build (concatinst 1 2)))

(displayln nfa2)
(newline)
(print-nfa nfa2)
(newline)
(print-nfa (set-nfa-closure-ref nfa2))
(newline)
(print-nfa (remove-eps nfa2))

(define nfa3 (build (starinst (orinst (concatinst 1 2) (concatinst 3 4)))))

(displayln nfa3)
(newline)
(print-nfa nfa3)
(newline)
(print-nfa (time (set-nfa-closure-ref nfa3)))
(newline)
(print-nfa (time (remove-eps nfa3)))

(define nfa4 (build (concatinst
                     (starinst
                      (orinst 1 2))
                     (concatinst
                      1
                      (concatinst
                       2
                       (concatinst
                        2
                        (starinst
                         (orinst 1 2))))))))


(displayln nfa4)
(newline)
(print-nfa nfa4)
(newline)
(print-nfa (time (set-nfa-closure-ref nfa4)))
(newline)
(print-nfa (time (remove-eps nfa4)))

(epsfree-nfa-match (remove-eps nfa4) '(1 1 2))
(epsfree-nfa-match (remove-eps nfa4) '(1 1 2 2 2 2 1 2 1))