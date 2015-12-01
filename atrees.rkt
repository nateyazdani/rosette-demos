#lang s-exp rosette

(permissive-xexprs #t)

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         xml
         "./util.rkt")

(struct atree (attributes children) #:transparent)

(define empty (atree null null))

(define (lookup symbol attributes)
  (cdr (findf (λ (attribute)
                (eq? symbol (car attribute)))
              attributes)))

(define (preorder f t)
  (let* ([tree (f t)]
         [attributes (atree-attributes tree)]
         [children (atree-children tree)])
    (atree attributes
           (map (λ (child) (preorder f child))
                children))))

(define (postorder f t)
  (let ([attributes (atree-attributes t)]
        [children (atree-children t)])
  (f (atree attributes
            (map (λ (child) (postorder f child))
                 children)))))

(define (atree* height width attr*)
  (if (= height 0)
      (atree (attr*) null)
      (atree (attr*)
             (mapthunk (thunk
                        (atree* (- height 1) width attr*))
                       width))))

(define (xyz-attr*)
   (list (cons 'x (hole*))
         (cons 'y (hole*))
         (cons 'z (hole*))))

(define (sum-xyz tree)
  (let* ([children (atree-children tree)]
         [attributes (atree-attributes tree)]
         [x (lookup 'x attributes)]
         [y (lookup 'y attributes)]
         [z (lookup 'z attributes)])
    (atree (cons (cons 'x+y+z (+ x y z))
                 attributes)
           children)))

(define (capture f . xs)
  (with-handlers ([(λ (_) #t)
                   (λ (x) x)])
    (apply f xs)))

(define (verify-traverse f h w)
  (verify 
   (let* ([tree (atree* h w xyz-attr*)]
          [pre (preorder f tree)]
          [post (postorder f tree)])
     (assert (equal? pre post)))))