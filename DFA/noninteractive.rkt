#lang rosette/safe

(require rosette/lib/synthax
         rosette/lib/match
         rosette/lib/angelic)

(require "DFA.rkt"
         "sketch.rkt"
         "oracle.rkt")

(define (synth-by-ref charset numofstate maxstrlen ref)
  (define sk (??sketch* numofstate charset))
  (define strsk (??string* charset maxstrlen))
  (let ([model (synthesize #:forall (symbolics strsk)
                           #:guarantee (assert (not (xor (dfa-match ref strsk) (dfa-match sk strsk)))))])
    (if (unsat? model)
        #f
        (evaluate sk (complete-solution model (symbolics sk))))))

(time (synth-by-ref '(a b) 4 8 oracle1))
(time (synth-by-ref '(a b c d) 5 8 oracle2))