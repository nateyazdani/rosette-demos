#lang s-exp rosette

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         "util.rkt")

(struct btree (left node right) #:transparent)
(define empty 'empty)

(define (height t)
  (if (eq? t empty)
      0
      (+ 1
         (max (height (btree-left t))
              (height (btree-right t))))))

(define (gather-preorder tree)
  (letrec ([traverse (λ (tree tail)
                       (if (eq? tree empty)
                           tail
                           (cons (btree-node tree)
                                 (traverse (btree-left tree)
                                           (traverse (btree-right tree) tail)))))])
    (traverse tree null)))

(define (gather-inorder tree)
  (letrec ([traverse (λ (tree tail)
                       (if (eq? tree empty)
                           tail
                           (traverse (btree-left tree)
                                     (cons (btree-node tree)
                                           (traverse (btree-right tree) tail)))))])
    (traverse tree null)))

(define (gather-postorder tree)
  (letrec ([traverse (λ (tree tail)
                       (if (eq? tree empty)
                           tail
                           (traverse (btree-left tree)
                                     (traverse (btree-right tree)
                                               (cons (btree-node tree) tail)))))])
    (traverse tree null)))

(define (reduce-preorder f z t)
  (if (eq? t empty)
      z
      (f (btree-node t)
         (reduce-preorder f z (btree-left t))
         (reduce-preorder f z (btree-right t)))))

(define (reduce-inorder f z t)
  (if (eq? t empty)
      z
      (f (reduce-inorder f z (btree-left t))
         (btree-node t)
         (reduce-inorder f z (btree-right t)))))

(define (reduce-postorder f z t)
  (if (eq? t empty)
      z
      (f (reduce-postorder f z (btree-left t))
         (reduce-postorder f z (btree-right t))
         (btree-node t))))

(define (btree* height node*)
  (if (> height 0)
      (btree (btree* (- height 1) node*)
             (node*)
             (btree* (- height 1) node*))
      empty))

(define (arith-mean xs)
  (/ (apply + xs)
     (length xs)))

(define (geo-mean xs)
  (expt (apply * xs)
        (/ (length xs))))

(define (sum xs)
  (apply + xs))

(define (product xs)
  (apply * xs))

(define (verify-gather h f)
  (verify
   (let* ([t (btree* h hole*)]
          [x (f (gather-preorder t))]
          [y (f (gather-inorder t))]
          [z (f (gather-postorder t))])
     (assert (= x y))
     (assert (= y z)))))

(define (capture f . xs)
  (with-handlers ([(λ (_) #t)
                   (λ (x) x)])
    (apply f xs)))

(define (verify-reduce h f z)
  (verify
   (let* ([t (btree* h hole*)]
          [x (reduce-preorder f z t)]
          [y (reduce-inorder f z t)]
          [z (reduce-postorder f z t)])
     (assert (= x y))
     (assert (= y z)))))