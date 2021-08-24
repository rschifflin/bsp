(define-module (bsp list)
               #:use-module (srfi srfi-1)
               #:use-module (bsp fn)
               #:export (all? dedup dedup-hash))
(define (all? pred lst)
  (cond [(null? lst) #t]
        [(not (pred (car lst))) #f]
        [else (all? pred (cdr lst))]))

;;; Given a list of items and a fn to cmp items
;;; cmp takes two items to compare for equality
;;; Return a dedup'd list of the items
(define (dedup lst cmp)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) lst]
        [(find (lambda (other) (cmp (car lst) other)) (cdr lst))
         (dedup (cdr lst) cmp)]
        [else (cons (car lst) (dedup (cdr lst) cmp))]))

;;; Given a list of items, a fn to hash items to indices, and a fn to cmp items
;;; hsh takes key, tablesize
;;; cmp takes two items to compare for equality
;;; Return a dedup'd list of the items
(define (dedup-hash lst hsh cmp)
  (let ((table (make-hash-table (length lst)))
        (search (lambda (key alist)
                  (find (lambda (pair) (cmp key (car pair))) alist))))
    (for-each (lambda (elt)
                (hashx-set! hsh search table elt elt)) lst)
    (hash-map->list first-arg table)))
