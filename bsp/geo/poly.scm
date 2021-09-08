;; Trait for working w/ polys
(define-module (bsp geo poly)
               #:use-module (bsp sewer trait)
               #:export (<poly> area))

(define-trait <poly> (;; Example trait fn; returns the value of the poly's area
                      (area (self <self>))
                      ))
