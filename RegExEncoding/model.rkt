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