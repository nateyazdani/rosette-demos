#lang s-exp rosette/safe

(require rosette/lib/meta/meta
         rosette/lib/tools/render)

(provide domain
         mapthunk
         choose*
         decide*
         number*
         hole*)

; -----------------------
; Random utility function
; -----------------------

(define (mapthunk f n)
    (letrec ([rec (lambda (i) (if (= i 0)
                                  null
                                  (cons (f)
                                        (rec (- i 1)))))])
      (rec n)))

(define (repeat x n)
    (letrec ([rec (lambda (i) (if (= i 0)
                                  null
                                  (cons x
                                        (rec (- i 1)))))])
      (rec n)))

; -------------------------
; Enhanced solver interface
; -------------------------

; Choose an expression dynamically
(define (choose* . xs)
  (define-symbolic* i number?)
  (assert (>= i 0))
  (assert (< i (length xs)))
  (list-ref xs i))

; Dynamically create a decision/boolean
(define (decide*)
  (define-symbolic* b boolean?)
  b)

; Dynamically create a number
(define (number* a b)
  (define-symbolic* n number?)
  (assert (>= n a))
  (assert (< n b))
  n)

; Dynamically create a constant hole
(define (hole*)
  (define-symbolic* h number?)
  h)

; Return the "domain" of the solver
(define domain
  (thunk
   (let ([n (expt 2 (- (current-bitwidth) 1))])
     (list
      (- n)
      (- n 1)))))