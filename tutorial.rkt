#lang s-exp rosette/safe

(require rosette/lib/meta/meta
         rosette/lib/tools/render)

(current-bitwidth 8)

(define (polynomial x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))
(define (factored x)
  (begin
    (define-symbolic a b c number?)
    (assert (not (< a 0)))
    (assert (not (< b 0)))
    (assert (not (< c 0)))
    (* (+ x a) (+ x 1) (+ x b) (+ x c))))

(define-symbolic i number?)
(define factor-synthesis
  (synthesize #:forall (list i)
              #:guarantee (assert (eq? (polynomial i) (factored i)))))

(define (!= x y)
  (not (= x y)))

(define (bound x min max)
  (and (> x min) (< x max)))

(define (constrain x)
  (bound x 0 (expt 2 5)))
(define-symbolic a b c d e number?)
(assert (constrain a))
(assert (constrain b))
(assert (constrain c))
(assert (constrain d))
(assert (constrain e))
(assert (= (/ (* d e) e) d))
(assert (= (/ (* d e) d) e))
(assert (= b (+ d e)))
(assert (= c (* d e)))

(define (f x)
  (+ (* x x a) (* x b) c))
(define (g x)
  (* (+ x d) (+ x e)))

(define-symbolic x number?)
(define coefficient-synthesis
  (synthesize #:forall (list x)
              #:guarantee (assert (= (f x) (g x)))))

(define (symlist n)
   (if (= n 0)
       null
       (begin
         (define-symbolic x number?)
         (cons x (symlist (- n 1))))))

(define (symlist* n) ; a list of distinct symbolic values
   (if (= n 0)
       null
       (begin
         (define-symbolic* x number?)
         (cons x (symlist* (- n 1))))))

(define good-list (symlist* 4))

(define good-solution
  (solve (assert (equal? good-list (list 1 2 3 4)))))