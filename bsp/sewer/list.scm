(define-module (bsp sewer list)
               #:use-module (srfi srfi-1)
               #:use-module (bsp sewer fn)
               #:export (all?
                          any?
                          dedup
                          dedup-hash
                          flat-map
                          runs))
(define (all? pred lst)
  (cond [(null? lst) #t]
        [(not (pred (car lst))) #f]
        [else (all? pred (cdr lst))]))

(define (any? pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) #t]
        [else (any? pred (cdr lst))]))

;;; Given a list of items and a fn to cmp items
;;; cmp takes two items to compare for equality
;;; Return a dedup'd list of the items
(define (dedup lst cmp)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) lst]
        [(find (lambda (other) (cmp (car lst) other)) (cdr lst))
         (dedup (cdr lst) cmp)]
        [else (cons (car lst) (dedup (cdr lst) cmp))]))

;; Return a list of runs, consecutive elements where each element subsequently satifies the predicate
;; compared with the previous element.
;; Once a subsequent element does not satisfy the predicate, it becomes the first element in a new run
;; Ie (runs = '(0 1 1 1 2 3 3 4 4 4)) => '((0) (1 1 1) (2) (3 3) (4 4 4))
(define (runs pred lst)
  (if (null? lst)
      '()
      (let self ((lst (cdr lst)) (current-run `(,(car lst))) (runs '()))
        (if (null? lst)
            (reverse (cons (reverse current-run) runs))
            (if (pred (car lst) (car current-run))
                (self (cdr lst) (cons (car lst) current-run) runs)
                (self (cdr lst) `(,(car lst)) (cons current-run runs)))))))

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

(define (flat-map f lst)
  (define (prepend x y) (append y x))
  (fold prepend '() (map f lst)))
