#lang rosette/safe

(provide (all-defined-out))

(require "DFA.rkt")

(define oracle1 (dfa (list (dfanode #f '((a . 1) (b . 0)))
                           (dfanode #f '((a . 1) (b . 2)))
                           (dfanode #f '((a . 1) (b . 3)))
                           (dfanode #t '((a . 1) (b . 0))))))
(define oracle2 (dfa (list (dfanode #t '((a . 1) (c . 2)))
                           (dfanode #f '((b . 3)))
                           (dfanode #f '((d . 4)))
                           (dfanode #t '((a . 1) (c . 2)))
                           (dfanode #t '((a . 1) (c . 2))))))


(define (oracle-read-func oracle)
  (lambda (str) (let ([ret (dfa-match oracle str)]) (begin (display str) (display ": ") (displayln ret) ret))))

