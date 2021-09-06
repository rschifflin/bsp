(define-module (bsp geo line)
               #:use-module (bsp geo vec3)
               #:export (make-line
                         make-line-from-points
                         line-point
                         line-dir))

(define (make-line point dir)
  (cons point dir))

(define (make-line-from-points p0 p1)
  ;; Tricky edge case:
  ;; A line from a->b might be subtly different than b->a due to floating point error.
  ;; Therefore we use a strict ordering of points so that we always return the same line
  ;; for the same two points
  (make-line p0 (v3:norm (apply v3:sub (v3:ord > p0 p1)))))

(define (line-point line)
  (car line))

(define (line-dir line)
  (cdr line))
