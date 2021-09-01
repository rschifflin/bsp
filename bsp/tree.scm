(define-module (bsp tree)
               #:export (make-tree
                         tree-datum
                         tree-children
                         tree-set-datum!
                         tree-lhs
                         tree-rhs
                         tree-leaf?))

(define (make-tree datum . children)
  (cons datum children))

(define tree-datum car)
(define tree-children cdr)

(define tree-set-datum! set-car!)

(define (tree-leaf? tree)
  (null? (tree-children tree)))

;; Useful aliases for common binary tree functions
(define (tree-lhs tree)
  (list-ref (tree-children tree) 0))

(define (tree-rhs tree)
  (list-ref (tree-children tree) 1))
