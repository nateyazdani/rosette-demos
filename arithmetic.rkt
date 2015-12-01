#lang s-exp rosette/safe

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         "util.rkt")

(provide answer
         approx
         symbol)

; ----------------------------------------
; Overflow-safe arithmetic (unusably slow)
; ----------------------------------------

; Calculate high bit of a positive integer.
(define (high-bit i)
  (define (rec n b)
    (if (= n 0)
        b
        (rec (>> n 1) (+ b 1))))
  (rec (abs i) 0))
;
;(define (add . xs)
;  (let ([b0 (current-bitwidth)]
;        [b1 (+ 2 (high-bit (apply max (map abs xs))))])
;    (current-bitwidth (max b0 b1))
;    (let ([s (apply + xs)])
;      (current-bitwidth (max b0 (+ (high-bit s) 1)))
;      s)))
;
;(define (sub . xs)
;  (apply - xs))
;
;(define (mul . xs)
;  (let ([b0 (current-bitwidth)]
;        [b1 (apply + (map high-bit (remove* (list 1) xs)))])
;    ; Using a crude approximation of the number of bits in the
;    ; product, act/warn on potential arithmetic overflow.
;    (when (<= b0 b1)
;          (current-bitwidth b1))
;         ;(display "WARNING: possible overflow\n"))
;    (let ([p (apply * xs)])
;      (current-bitwidth (max b0 (+ (high-bit p) 1)))
;      p)))
;
;(define (div . xs)
;  ; We really can't do much to avoid precision/accuracy loss here
;  (apply / xs))
;
;(define (pow x y)
;  ; This is just a bad idea no matter what...
;  (expt x y))

; -----------------------
; Solver-aided procedures
; -----------------------

; A symbolic constant in some range, i.e., an inequality
(define-syntax-rule (symbol x [xmin xmax])
  (begin
    (define-symbolic x number?)
    (let ([a xmin]
          [b xmax])
      (current-bitwidth
       (max
        (current-bitwidth)
        (high-bit a)
        (high-bit b)))
      (assert (>= x a))
      (assert (<= x b))
      (asserts)
      x)))

; Resolve any and all unqualified symbols
(define (answer es
                #:forall xs
                #:where ps)
  (synthesize #:forall xs
              #:assume (map (λ (p) (assert p)) ps)
              #:guarantee (map (λ (e) (assert e)) es)))

; Synthesize an approximate function, subject to some error bound, using
; an expression tree of the given depth  while preserving the given predicates.
; Unfortunately, the error bounding assertion tends to overflow, so the function
; has been modified to only synthesize exact replicate functions.
(define (approx f
                #:given xs
                #:where ps
;               #:error e
                #:depth d)
  (letrec ([ar (length xs)] ; arity of synthetic function
           [expr (λ (d)
                   (if (= d 0)
                       (if (decide*)
                           (let ([v (hole*)])
                             (λ _ v))
                           (let ([i (number* 0 ar)])
                             (λ xs (list-ref xs i))))
                       (let* ([op (choose* + - * /)]
                              [dn (- d 1)]
                              [e0 (expr dn)]
                              [e1 (expr dn)])
                         (λ xs (op (apply e0 xs) (apply e1 xs))))))]
           [g (expr d)]
           [sol (synthesize #:forall xs
                            #:assume (map (λ (p) (assert p)) ps)
                            #:guarantee (assert (= (apply f xs) (apply g xs))))])
;                            (assert (<= (abs (- (apply f xs) (apply g xs))) e)))])    
    (print sol)
    (newline)
    (λ xs (evaluate (apply g xs) sol))))

; -----------------------------
; Expression synthesis examples
; -----------------------------

(symbol x [-64 63])
(define f (approx (λ (x) (* x x))
                    #:given (list x)
                    #:where '()
                    #:depth 1))
(define g (approx (λ (x) (* (+ x 2) (+ x 2)))
                    #:given (list x)
                    #:where '()
                    #:depth 2))
(define h (approx (λ (x) (+ (* x x x) (* x x) (* 8 x) 16))
                    #:given (list x)
                    #:where '()
                    #:depth 3))
