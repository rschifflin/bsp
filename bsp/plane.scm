(define-module (bsp plane)
               #:use-module (bsp list)
               #:use-module (bsp vec3)
               #:use-module (bsp line)
               #:export (make-plane
                         make-plane-from-points
                         plane-point
                         plane-normal
                         plane-cmp-point
                         plane-test-face
                         plane-line-intersection))

(define (make-plane point normal)
  (cons point normal))

(define (make-plane-from-points p0 p1 p2)
  (make-plane p0 (v3:norm (v3:cross (v3:sub p1 p0)
                                    (v3:sub p2 p0)))))

(define (plane-point plane)
  (car plane))

(define (plane-normal plane)
  (cdr plane))

;;; Test a point on a plane
;;; Returns the sign of the point w/r/t the plane:
;;; = for planar, otherwise + or -
;;; NOTE: A tolerance epsilon may be wanted for points very nearly on the plane
(define (plane-cmp-point plane point)
  (let ((normal-component (v3:dot (v3:sub point (plane-point plane))
                                  (plane-normal plane)))
        (tolerance 0.0))
    (cond [(> normal-component tolerance) '+]
          [(< normal-component (-  tolerance)) '-]
          [else '=])))

;;; Test a polygon for intersection with a plane
;;; Returns ADT, one of:
;;;   '(#f =) means no intersection, planar
;;;   '(#f +) means no intersection, positive
;;;   '(#f -) means no intersection, negative
;;;   '(#t (<+|-|=>...)) means intersection, and provides a sign list of -/+/= for each point in the polygon
(define (plane-test-face plane points)
  (let* ((signs (map (lambda (point) (plane-cmp-point plane point))
                     points)))

      (cond [(all? signs (lambda (sign) (eq? sign '=)))
             '(#f =)]
            [(all? signs (lambda (sign) (or (eq? sign '=) (eq? sign '+))))
             '(#f +)]
            [(all? signs (lambda (sign) (or (eq? sign '=) (eq? sign '-))))
             '(#f -)]
            [else `(#t ,signs)])))

;;; Given a line that definitely intersects the plane,
;;; return the point of intersection.
(define (plane-line-intersection plane line)
  (let* ((l0 (line-point line))
         (l  (line-dir line))
         (p0 (plane-point plane))
         (n  (plane-normal plane))
         (s  (/ (v3:dot n (v3:sub p0 l0))
                (v3:dot n l))))

    (v3:sum l0 (v3:scale l s))))
