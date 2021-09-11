;; Provides higher-order functions for convenience
(define-module (bsp sewer fn)
               #:use-module (srfi srfi-1)
               #:export (applied
                          1*
                          2*
                          n*
                          first-arg
                          last-arg))

;; Returns a function which takes a list
;; and applies it as args to the provided function f.
(define (applied f)
  (lambda (arglist) (apply f arglist)))

;; Takes an arity-n fn and returns a fn that accepst and discards any extra args
(define (n* n fn)
  (lambda (. args)
    (apply fn (take args n))))

;; Takes a unary fn and returns a fn that accepts and discards any extra args
(define (1* fn) (n* 1 fn))

;; Takes an arity-2 fn and returns a fn that accepts and discards any extra args
(define (2* fn) (n* 2 fn))

;; Returns the first arg called with
(define (first-arg arg . ignored) arg)

;; Returns the last arg called with
(define (last-arg . args) (last args))
