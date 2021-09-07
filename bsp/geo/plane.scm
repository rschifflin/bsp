(define-module (bsp geo plane)
               #:use-module (srfi srfi-1) ;; zip
               #:use-module (srfi srfi-26)
               #:use-module (rnrs bytevectors)
               #:use-module (bsp sewer list)
               #:use-module (bsp geo consts)
               #:use-module (bsp geo vec3)
               #:use-module (bsp geo line)
               #:export (make-plane
                         make-plane-from-points
                         plane-point
                         plane-normal
                         plane-cmp-point
                         plane-cmp-line
                         plane-test-points
                         plane-test-intersects?
                         plane-test=
                         plane-test-sign
                         plane-test-signs
                         plane-line-intersection
                         plane-flip
                         plane->u8vector
                         plane=
                         plane~=))

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
(define (plane-cmp-point plane point)
  (let ((projection-length (v3:dot (v3:norm (v3:sub point (plane-point plane)))
                                   (plane-normal plane))))
    (cond [(> projection-length VECTOR_EPSILON) '+]
          [(< projection-length (- VECTOR_EPSILON)) '-]
          [else '=])))

(define (plane-flip p)
  (let ((point (plane-point p))
        (normal (plane-normal p)))
    (make-plane point
                (v3:neg normal))))

;;; True if two planes are roughly equal
(define (plane~= p1 p2)
  (and (v3:norm= (plane-normal p1) (plane-normal p2))
       (let* ((tangent (v3:norm (v3:sub (plane-point p2) (plane-point p1))))
              (projection-length (v3:dot tangent (plane-normal p1))))
         (and (< projection-length VECTOR_EPSILON)
              (> projection-length (- VECTOR_EPSILON))))))

(define (plane->u8vector p)
  (let* ((point-u8vector (vec3->u8vector (plane-point p)))
         (normal-u8vector (vec3->u8vector (plane-normal p)))
         (point-len (u8vector-length point-u8vector))
         (normal-len (u8vector-length normal-u8vector))
         (plane-u8vector (make-u8vector (+ point-len normal-len))))
    (bytevector-copy! point-u8vector 0 plane-u8vector 0 point-len)
    (bytevector-copy! normal-u8vector 0 plane-u8vector point-len normal-len)
    plane-u8vector))

;;; Test a line on a plane
;;; Returns the intersection of the line w/r/t the plane:
;;;   'none if no intersections
;;;   'inf if infinitely many intersections (line and plane coincide)
;;;   vec3 of the intersection point otherwise
(define (plane-cmp-line plane line)
  (let* ((l0 (line-point line))
         (l  (line-dir line))
         (p0 (plane-point plane))
         (n  (plane-normal plane))
         (n-dot-p (v3:dot n (v3:sub p0 l0)))
         (n-dot-l (v3:dot n l)))
    (if (= n-dot-l 0.0) ; Parallel case, either no intersections or infinite
        (if (= n-dot-p 0.0) 'inf 'none)
        (let ((scale (/ n-dot-p n-dot-l))) ; Otherwise, exactly 1 intersection
          (v3:sum l0 (v3:scale l scale))))))

;;; Test a polygon for intersection with a plane
;;; Returns ADT, one of:
;;;   '(#f =) means no intersection, planar
;;;   '(#f +) means no intersection, positive
;;;   '(#f -) means no intersection, negative
;;;   '(#t (<+|-|=>...)) means intersection, and provides a sign list of -/+/= for each point in the polygon
;;; This test uses an upper and lower bounding plane. Points that are within the bounding plane are considered 'on the plane'.
;;; This prevents us from clipping planes that are infinitesimally intersected, but would otherwise be unclipped.
;;; If plane-poly intersection is still found, the list of signs will be generated using the exact plane.
(define (plane-test-points plane points)
  (let* ((halfwidth (v3:scale (plane-normal plane) PLANE_HALFWIDTH))
         (upper-plane (make-plane (v3:sum (plane-point plane) halfwidth) (plane-normal plane)))
         (lower-plane (make-plane (v3:sub (plane-point plane) halfwidth) (plane-normal plane))))
    (let* ((upper-signs (map (cut plane-cmp-point upper-plane <>) points))
           (lower-signs (map (cut plane-cmp-point lower-plane <>) points))
           (bounded-signs (zip upper-signs lower-signs)))

      (cond [(all? (lambda (bounded-sign)
                     (let ((upper-sign (first bounded-sign))
                           (lower-sign (second bounded-sign)))
                       (and (or (eq? upper-sign '=) (eq? upper-sign '-))
                            (or (eq? lower-sign '=) (eq? lower-sign '+)))))
                   bounded-signs)
             '(#f =)]
            [(all? (lambda (upper-sign) (and (or (eq? upper-sign '=) (eq? upper-sign '-)))) upper-signs)
             '(#f -)]
            [(all? (lambda (lower-sign) (and (or (eq? lower-sign '=) (eq? lower-sign '+)))) lower-signs)
             '(#f +)]
            ;; Use the true signs for exact plane cutting in the split case
            [else `(#t ,(map (cut plane-cmp-point plane <>) points))]))))

(define (plane-test= test) (and (not (car test))
                                (eq? (cadr test) '=)))
(define plane-test-intersects? car)
(define plane-test-sign cadr)
(define plane-test-signs cadr)

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
