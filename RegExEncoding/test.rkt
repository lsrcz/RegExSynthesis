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
  (concat (??num) (concat (??num) (??num))))

(define x (??num))

(print-regex (evaluate x (complete-solution (solve-match-lst x '((4) (5)) #| (list (list 0) (list 1)) |# '((0) (3) (6)) 0) (symbolics x))))

(define ipsk (select (select (??h1) (??h1)) (??h1)))

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

(print-regex (evaluate ipsk ipmodel))