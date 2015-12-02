#lang s-exp rosette

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         "../../util.rkt")

; ------------------
; Automaton creation
; ------------------

; Generate an automaton, given a transition matrix, a symbol index function,
; an initial state, an accepting state, and a rejecting state.
(define (automaton sigma matrix initial final)
  (letrec ([index (λ (symbol)
                    (vector-member symbol sigma))]
           [delta (λ (state symbol)
                    (vector-ref (vector-ref matrix state)
                                (index symbol)))]
           [inter (λ (state stream)
                    (if (null? stream)
                        (= state final) ; empty input, is the state accepting?
                        (inter (delta state
                                      (first stream))
                               (rest stream))))])
    (λ (string) (inter initial (string->list string)))))

; -----------------
; Automaton example
; -----------------

(current-bitwidth 6)

; Manually written automaton to detect patterns matching /ca+r+l/
(define carl-man
    (automaton (vector #\c #\a #\r #\l)
               (vector (vector 1 5 5 5) ; start
                       (vector 5 2 5 5)
                       (vector 5 2 3 5)
                       (vector 5 5 3 4)
                       (vector 5 5 5 5) ; stop
                       (vector 5 5 5 5))
               0
               4))

; Generated automaton to detect the same patterns
;(define carl-gen
;  (regex "ca+r+l"))
;(carl-gen '(c a a a a a a a r r r r r l))

; -------------------
; Automaton synthesis
; -------------------

(define alphabet
  (list->vector (string->list "abcdefghijklmnopqrstuvwxyz")))

; Construct a symbolic automaton (one with a symbolic transitiuon matrix)
(define (automaton* sigma states)
  (let ([delta (build-vector states
                             (λ (_)
                               (build-vector (vector-length sigma)
                                             (λ (_)
                                               (number* 0 states)))))])
    (display "transition matrix: ")
    (displayln delta)
    (automaton sigma
               delta
               0
               (- states 1))))

; Synthesize an automaton to accept and reject the given examples.
(define (mechanize positive negative sigma states)
  (let* (
         [machine (automaton* sigma states)]
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
  (time
   (println "constructing symbolic automaton...")
   (mechanize (list "caaaarl"
                    "caaarl"
                    "caarl"
                    "carl"
                    "a")
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
                    ;'(c a a l l c) ; twelveth
                    ;'(a a a a l c) ; thirteenth
                    ;'(r a r l l r) ; fourteenth
                    ;'(c a a r l c) ; fifteenth
                    ;'(r l c l l l) ; sixteenth
                    ;'(l a a r l l) ; seventeenth
                    ;'(c l a r l l) ; eighteenth
                    ;'(c a r l l c) ; nineteenth
                    "ccarl"
                    "lrac"
                    "crl"
                    "cal"
                    "car"
                    "arl"
                    "cl"
                    "ccarl")
              (vector #\c #\a #\r #\l)
              6)))

; -----------------
; Residual programs
; -----------------

(define fsm (automaton* (vector #\a #\b) 2))

;original: (fsm "a")
;residual: (= 1 n$24)

;original: (fsm "ab")
;residual: (= 1 (bitwise-ior (ite (= 1 n$24) n$27 0)
;                            (ite (= 0 n$24) n$25 0)))