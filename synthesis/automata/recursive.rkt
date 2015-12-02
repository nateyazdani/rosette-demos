#lang s-exp rosette

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         "../../util.rkt")

; ------------------
; Automaton creation
; ------------------

; Generate an automoton, given an initial state, a list of accepting
; final states, and a list of state transitions. The initial and final
; states are identified by a natural number that is also the index into
; the list of state transitions that yields the associated state
; transition, which is itself a list of quoted character and state
; index pairs (or -1, to indicate rejection). In some sense, this can be
; considered a "deeply" embedded DSL, in that the program is specified
; as data (though not any sort of abstract syntax tree) and is
; interpreted in the host language.
(define (automaton initial final states)
  (letrec ([delta (apply vector states)]
           [inter (λ (index stream)
                    (letrec ([rec (λ (trans)
                                    (cond
                                      [(or (null? trans)
                                           (null? stream))
                                       (= index final)]
                                      [(equal? (car (first trans))
                                               (first stream))
                                       (inter (cdr (first trans)) (rest stream))]
                                      [else
                                       (rec (rest trans))]))])
                      (rec (vector-ref delta index))))])
    (λ (stream) (inter initial stream))))

; -----------------
; Automaton example
; -----------------

(current-bitwidth 6)

; Manually written automaton to detect patterns matching /ca+r+l/
(define carl-man
  (automaton 0
             5
             (list
              (list (cons 'c 1))
              (list (cons 'a 2))
              (list (cons 'a 2)
                    (cons 'r 3))
              (list (cons 'r 3)
                    (cons 'l 4))
              (list))))

; Generated automaton to detect the same patterns
;(define carl-gen
;  (regex "ca+r+l"))
;(carl-gen '(c a a a a a a a r r r r r l))

; -------------------
; Automaton synthesis
; -------------------

(define alphabet
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

; Construct a symbolic automaton with a symbolic delta function
(define (automaton* sigma states transitions)
  (letrec ([trans* (λ (t)
                     (if (= t 0)
                         null
                         (cons (cons (apply choose* sigma)
                                     (number* 0 states))
                               (trans* (- t 1)))))]
           [state* (λ (s)
                     (if (= s 0)
                         null
                         (cons (trans* transitions) (state* (- s 1)))))]
           [delta* (state* states)])
    (automaton 0 (- states 1) delta*)))

; Synthesize an automaton to accept and reject the given examples.
(define (mechanize positive negative sigma states transitions)
  (letrec ([machine (automaton* sigma states transitions)]
           [solution (time
                      (println "synthesizing concrete automaton...")
                      (solve (begin
                               (assert (andmap machine positive))
                               (assert (not (ormap machine negative))))))])
    (λ (s)
      (evaluate (machine s) solution))))

; ---------------------
; Synthesized automaton
; ---------------------

; Synthesize automaton to recognize patterns matching /ca+r+l/
(define carl-gen
  (time ;(with-handlers ([(λ (x) #t) (λ (x) x)])
   (println "constructing symbolic automaton...")
   (mechanize (list '(c a a a a r l)
                    '(c a a a r l)
                    '(c a a r l)
                    '(c a r l))
              (list ;'(c a r l r a) ; first verified counterexample
                    ;'(c a r r l l) ; second
                    ;'(r r l r l a) ; third
                    ;'(l l r l a a) ; fourth
                    ;'(a c a a r l) ; fifth
                    ;'(r a a a r c) ; sixth
                    ;'(c a r r c c) ; seventh
                    ;'(l a c c a c) ; eighth
                    ;'(c a c c r r) ; ninth
                    ;'(r l c a c c) ; tenth
                    ;'(r a a c a c) ; eleventh
                    ;'(c a a l l c) ; twelfth
                    ;'(a a a a l c) ; thirteenth
                    ;'(r a r l l r) ; fourteenth
                    ;'(c a a r l c) ; fifteenth
                    ;'(r l c l l l) ; sixteenth
                    ;'(l a a r l l) ; seventeenth
                    ;'(c l a r l l) ; eighteenth
                    ;'(c a r l l c) ; nineteenth
                    '(c c a r l)
                    '(l r a c)
                    '(c r l)
                    '(c a l)
                    '(c a r)
                    '(a r l)
                    '(c l)
                    '(c c a r l))
              '(c a r l)
              6
              2)));)


; -----------------
; Residual programs
; -----------------

(define fsm (automaton* '(a b) 2 1))

;original: (fsm '(a))
;residual: (&& (= 0 i$12)
;              (= 1 n$12))

;original: (fsm '(a b))
;residual: (&& (= 0 i$12)
;              (|| (&& (= 1 n$12)
;                      (! (|| (&& (= 1 i$12)
;                                 (= 0 n$12))
;                             (&& (= 1 i$13)
;                                 (= 1 n$12)))))
;                  (&& (|| (&& (= 1 i$12)
;                              (= 0 n$12))
;                          (&& (= 1 i$13)
;                              (= 1 n$12)))
;                      (= 1 (bitwise-ior (ite (= 1 n$12) n$13 0)
;                                        (ite (= 0 n$12) n$12 0))))))
