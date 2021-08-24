;;; Hash helpers
(define-module (bsp hash)
               #:use-module (srfi srfi-1)
               #:export (hash-bytes))

;;; Simple hash over byte lists
(define (hash-bytes size lst1 . rest)
  (define (roll-hash next-val prev-val)
    (remainder
      (+  (* 3 (hash prev-val size))
          (hash next-val size))
      size))
  (define (hash-lst lst seed)
    (fold roll-hash seed lst))

  (fold hash-lst (hash-lst lst1 0) rest))
