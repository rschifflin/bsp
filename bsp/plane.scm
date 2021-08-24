(define-module (bsp plane)
               #:use-module (rnrs bytevectors)
               #:use-module (bsp list)
               #:use-module (bsp vec3)
               #:use-module (bsp line)
               #:export (make-plane
                         make-plane-from-points
                         plane-point
                         plane-normal
                         plane-cmp-point
                         plane-cmp-line
                         plane-test-face
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

;;; TODO: Use a proper epsilon comparison
(define TOLERANCE 0.0001)

;;; Test a point on a plane
;;; Returns the sign of the point w/r/t the plane:
;;; = for planar, otherwise + or -
(define (plane-cmp-point plane point)
  (let ((normal-component (v3:dot (v3:sub point (plane-point plane))
                                  (plane-normal plane)))
        (TOLERANCE 0.0001))
    (cond [(> normal-component TOLERANCE) '+]
          [(< normal-component (- TOLERANCE)) '-]
          [else '=])))

(define (plane-flip p)
  (let ((point (plane-point p))
        (normal (plane-normal p)))
    (make-plane point
                (make-vec3 (- (@x normal))
                           (- (@y normal))
                           (- (@z normal))))))

;;; True if two planes are equal
(define (plane= p1 p2)
  (and (v3:= (plane-normal p1) (plane-normal p2))
       (let* ((difference (v3:sub (plane-point p2) (plane-point p1)))
              (dot (v3:dot difference (plane-normal p1))))
         (= dot 0))))

;;; True if two planes are equal, allowing normals with opposite direction
(define (plane~= p1 p2)
  (or (plane= p1 p2)
      (plane= p1 (plane-flip p2))))

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
(define (plane-test-face plane points)
  (let* ((signs (map (lambda (point) (plane-cmp-point plane point))
                     points)))

      (cond [(all? (lambda (sign) (eq? sign '=)) signs)
             '(#f =)]
            [(all? (lambda (sign) (or (eq? sign '=) (eq? sign '+))) signs)
             '(#f +)]
            [(all? (lambda (sign) (or (eq? sign '=) (eq? sign '-))) signs)
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
