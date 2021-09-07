(define-module (bsp sewer display)
               #:export (println))

(define (println . xs)
  (for-each display xs)
  (newline))
