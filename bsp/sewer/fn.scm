;; Provides higher-order functions for convenience
(define-module (bsp sewer fn)
               #:export (applied first-arg))

;; Returns a function which takes a list
;; and applies it as args to the provided function f.
(define (applied f)
  (lambda (arglist) (apply f arglist)))

;; Returns the first arg called with
(define (first-arg arg . ignored) arg)
