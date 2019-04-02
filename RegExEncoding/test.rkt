#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/angelic
         rosette/lib/synthax)

(require "match.rkt"
         "model.rkt"
         "synth.rkt"
         "sketch.rkt")


(define (??h1)
  (choose* (concat (??num) (concat (??num) (??num)))
           (concat (??num) (??num))
           (??num)))

(define x (??num))

(print-regex (evaluate x (complete-solution (solve-match-lst x '((4) (5)) #| (list (list 0) (list 1)) |# '((0) (3) (6)) 0) (symbolics x))))

(define ipsk (select (select (select (select (??h1) (??h1)) (??h1)) (??h1)) (??h1)))

(define ipmodel (time (complete-solution
                       (solve-match-lst ipsk
                                        (list '(0)
                                              '(9)
                                              '(1 0)
                                              '(9 9)
                                              '(1 0 0)
                                              '(1 9 9)
                                              '(1 4 5)
                                              '(1 5 6)
                                              '(2 0 0)
                                              '(2 0 6)
                                              '(2 4 9)
                                              '(2 5 0)
                                              '(2 5 5))
                                        (list '(0 1)
                                              '(0 1 1)
                                              '(0 0 1)
                                              ;'(0 0 9)
                                              '(2 5 6)
                                              '(2 5 9)
                                              '(2 6 0)
                                              '(2 9 4)
                                              ;'(3 0 0)
                                              '(3 0 6)
                                              '(3 5 0)
                                              '(3 5 5)
                                              '(3 5 6)
                                              '(9 9 9))
                                        0)
                       (symbolics ipsk))))

;[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]

(print-regex (evaluate ipsk ipmodel))

(define (??numstar)
  (choose* (??num) (star (??num))))

(define (??numstar2)
  (choose* (??numstar) (concat (??numstar) (??numstar))))

(define noleadingsk
  (choose* (??numstar2)
           (select (??numstar2) (??numstar2))))

(length (symbolics noleadingsk))
(define noleadingmodel (time (complete-solution
                              (solve-match-lst noleadingsk
                                               (list '(0)
                                                     '(9)
                                                     '(1 0)
                                                     '(9 9)
                                                     '(1 0 0)
                                                     '(1 0 1)
                                                     '(9 9 9))
                                               (list '(0 1)
                                                     '(0 1 1)
                                                     '(0 0 1)
                                                     '(0 0))
                                               3)
                              (symbolics noleadingsk))))

(print-regex (evaluate noleadingsk noleadingmodel))

