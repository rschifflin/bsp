(define-module (bsp sewer tree)
               #:use-module (srfi srfi-1)
               #:use-module (bsp sewer fn)
               #:use-module (bsp sewer list)
               #:use-module (bsp sewer plist)
               #:export (make-tree
                         tree-null
                         tree-datum
                         tree-children
                         tree-null?
                         tree-leaf?
                         tree-set-datum!
                         tree-lhs
                         tree-rhs
                         tree-map
                         tree-fold-both
                         tree-for-each
                         tree-length
                         tree->vector))

(define tree-null '())
(define tree-null? null?)
(define (tree-not-null? tree) (not (tree-null? tree)))

(define (make-tree datum . children)
  (cons datum children))

(define tree-datum car)
(define tree-children cdr)
(define tree-set-datum! set-car!)

(define (tree-leaf? tree)
  (and (not (null? tree))
       (all? tree-null? (tree-children tree))))

;; Useful aliases for common binary tree functions
(define (tree-lhs tree)
  (list-ref (tree-children tree) 0))

(define (tree-rhs tree)
  (list-ref (tree-children tree) 1))

;; Both-order traversal
(define* (tree-fold-both fn-pre fn-post init tree #:optional visit-nulls?)
  (let ((fn-null (if visit-nulls?
                     (lambda (acc) (fn-post #f (fn-pre #f acc)))
                     identity))
        (tree-datum (if visit-nulls?
                        (lambda (tree) (cons 'datum (tree-datum tree)))
                        tree-datum)))
    (let tree-fold* ((tree tree) (acc init))
      (if (tree-null? tree)
          (fn-null acc)
          (let* ((acc-pre (fn-pre (tree-datum tree) acc))
                 (acc-post (fold tree-fold* acc-pre (tree-children tree))))
            (fn-post (tree-datum tree) acc-post))))))

(define* (tree-fold-pre fn init tree #:optional visit-nulls?)
  (tree-fold-both fn last-arg init tree visit-nulls?))

(define* (tree-fold-post fn init tree #:optional visit-nulls?)
  (tree-fold-both last-arg fn init tree visit-nulls?))

(define tree-fold tree-fold-pre)

(define (tree-map fn tree)
  (let tree-map* ((tree tree))
    (if (tree-null? tree) tree-null
        (apply make-tree (cons (fn (tree-datum tree))
                               (map tree-map* (tree-children tree)))))))

(define (tree-for-each fn tree)
  (let tree-for-each* ((tree tree))
    (if (tree-not-null? tree)
        (begin
          (fn (tree-datum tree))
          (for-each tree-for-each* (tree-children tree))))))

(define (tree-length tree)
  (tree-fold (lambda (_data acc) (+ 1 acc)) 0 tree))

;; Produces a vector whose elements represent nodes in the tree
;; Each element is a plist where 'datum holds the datum of the node
;; and 'children holds a list of vector indices containing the child nodes
(define (tree->vector tree)
  (let ((v (make-vector (tree-length tree))))
    ;; Record the current index on the stack
    (define (pre _ state)
      (let ((counter (pget state 'counter))
            (stack (pget state 'stack)))
        (list 'counter (+ counter 1)
              'stack (cons counter (cons 'delim stack)))))

    ;; Pull the lastest children and index off the stack, write it, and push the index back onto the stack
    (define (post datum state)
      (let ((counter (pget state 'counter))
            (stack (pget state 'stack)))
        (let self ((stack stack) (popped '()))
          (if (not (eq? 'delim (car stack))) ;; Until we hit a delimiter...
              (self (cdr stack) (cons (car stack) popped)) ;; ...Keep popping elements off the stack
              (begin
                ;; Popped holds index, child1, child2, ..., childn
                ;; Perform write to V
                (vector-set! v (car popped) (list 'datum datum 'children (cdr popped)))
                ;; Push our index back onto the stack without the delimiter, to be counted as a child for the next parent
                (list 'counter counter
                      'stack (cons (car popped) (cdr stack))))))))
    (tree-fold-both pre post (list 'counter 0 'stack '()) tree)
    v))
