#lang s-exp rosette/safe

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         "../util.rkt")

; -------------------
; Utility definitions
; -------------------

(define alphabet
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define letter*
  (thunk
    (apply choose* alphabet)))

; ----------------
; Automaton syntax
; ----------------

(define-syntax automaton
  (syntax-rules (:)
    [(_ initial
        (state : response ...)
        ...)
     (let-syntax
         ([process-state
           (syntax-rules (accept ->)
             [(_ accept)
              (λ (stream)
                (cond
                  [(empty? stream) true]
                  [else false]))]
             [(_ (label -> target) (... ...))
              (λ (stream)
                (cond
                  [(empty? stream) false]
                  [else
                   (case (first stream)
                     [(label) (target (rest stream))]
                     (... ...)
                     [else false])]))])])
       (letrec ([state
                 (process-state response ...)]
                ...)
         initial))]))

; ----------------
; Automaton sketch
; ----------------

(define sketch
  (automaton s0
             [s0 : (c -> (choose s0 s1 s2 s3 s4 s5 s6))
                   (k -> (choose s1 s2 s3 s4 s5 s6))]
             [s1 : (a -> (choose s1 s2 s3 s4 s5 s6))]
             [s2 : (a -> (choose s1 s2 s3 s4 s5 s6 s7))
                   (t -> (choose s1 s2 s3 s4 s5 s6 s7))]
             [s3 : (i -> (choose s1 s2 s3 s4 s5 s6 s7))]
             [s4 : (i -> (choose s1 s2 s3 s4 s5 s6 s7))
                   (t -> (choose s1 s2 s3 s4 s5 s6 s7))]
             [s5 : (t -> (choose s1 s2 s3 s4 s5 s6 s7))]
             [s6 : (y -> (choose s1 s2 s3 s4 s5 s6 s7))]
             [s7 : accept]))

(define synthesized
 (time (solve 
         (begin
          (assert (sketch '(c a a t)))
          (assert (sketch '(c a a a t)))
          (assert (not (sketch '(c t))))
          (assert (sketch '(k i t t y)))
          (assert (sketch '(k i i i i t t y)))
          (assert (not (sketch '(k t t y))))
          (assert (not (sketch '(k i t y))))))))