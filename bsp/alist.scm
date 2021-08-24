(define-module (bsp alist)
               #:use-module (srfi srfi-1)
               #:export (alist?
                         aref
                         aset!
                         amerge))

;; Predicate for determining if a list is an alist
(define (alist? lst)
  (if (null? lst)
      #t
      (if (pair? (car lst))
          (alist? (cdr lst))
          #f)))

;; Helpers for alists using eq?
(define aref assq-ref)
(define aset! assq-set!)

;; Modifies target-alist to contain the keys from replacement-alist
(define (amerge target-alist replacement-alist)
  (fold (lambda (replacement-pair target-alist)
          (aset! target-alist (car replacement-pair) (cdr replacement-pair)))
        (alist-copy target-alist)
        replacement-alist))
