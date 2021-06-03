;; Provides higher-order functions for convenience
(define-module (bsp fn)
               #:export (applied))

;; Returns a function which takes a list
;; and applies it as args to the provided function f.
(define (applied f)
  (lambda (arglist) (apply f arglist)))
