(define-module (bsp line)
               #:use-module (bsp vec3)
               #:export (make-line
                         make-line-from-points
                         line-point
                         line-dir))

(define (make-line point dir)
  (cons point dir))

(define (make-line-from-points p0 p1)
  (make-line p0 (v3:norm (v3:sub p1 p0))))

(define (line-point line)
  (car line))

(define (line-dir line)
  (cdr line))
