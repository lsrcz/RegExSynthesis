#lang rosette/safe

(provide (all-defined-out))

(require rosette/lib/match
         rosette/lib/synthax
         rosette/lib/angelic)
(define (println x) (begin (print x) (newline)))
(struct orinst (left right) #:transparent)
(struct concatinst (left right) #:transparent)
(struct starinst (base) #:transparent)

(struct nnode (idx nxt eps) #:transparent)
(struct nfa (start end lst) #:transparent)


(define (nnode-lst-add-eps sourcelst targetlst nnodelst)
  (map (lambda (n) (if (member (nnode-idx n) sourcelst)
                       (nnode (nnode-idx n) (nnode-nxt n) (append targetlst (nnode-eps n)))
                       n)) nnodelst))

(define (build-simple symbol minidx)
  (cons (nfa minidx (+ 1 minidx) (list (nnode minidx
                                        (list (cons symbol (list (+ 1 minidx))))
                                        '())
                                       (nnode (+ 1 minidx)
                                             '()
                                             '()))) (+ 2 minidx)))

(define (build-or left right minidx)
  (cons (nfa minidx
             (+ 1 minidx)
             (append (list (nnode minidx '()
                            (list (nfa-start left) (nfa-start right)))
                           (nnode (+ 1 minidx) '() '()))
                     (nnode-lst-add-eps (list (nfa-end left) (nfa-end right)) (list (+ 1 minidx))
                                       (append (nfa-lst left) (nfa-lst right)))))
        (+ 2 minidx)))

(define (single-add-eps lidx ridxlst llst)
  (if (null? llst)
      #f
      (if (= lidx (nnode-idx (car llst)))
          (cons (nnode lidx (nnode-nxt (car llst)) (append ridxlst (nnode-eps (car llst)))) (cdr llst))
          (cons (car llst) (single-add-eps lidx ridxlst (cdr llst))))))

(define (build-concat left right minidx)
  (cons (nfa (nfa-start left)
             (nfa-end right)
             (append (single-add-eps (nfa-end left) (list (nfa-start right)) (nfa-lst left)) (nfa-lst right))) minidx))

(define (build-star base minidx)
  (cons (nfa minidx
             (+ 1 minidx)
             (append (list (nnode minidx '() (list (nfa-start base) (+ 1 minidx)))
                           (nnode (+ 1 minidx) '() '()))
                     (single-add-eps (nfa-end base) (list (nfa-start base) (+ 1 minidx)) (nfa-lst base)))) (+ 2 minidx)))

(define (build-nfa inst minidx)
  (match inst
    [(orinst a b)
     (let* ([lret (build-nfa a minidx)]
            [rret (build-nfa b (cdr lret))])
       (build-or (car lret) (car rret) (cdr rret)))]
    [(concatinst a b)
     (let* ([lret (build-nfa a minidx)]
            [rret (build-nfa b (cdr lret))])
       (build-concat (car lret) (car rret) (cdr rret)))]
    [(starinst a)
     (let* ([ret (build-nfa a minidx)])
       (build-star (car ret) (cdr ret)))]
    [_ (build-simple inst minidx)]))

(define (build inst)
  (car (build-nfa inst 0)))
