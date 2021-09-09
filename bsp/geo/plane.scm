(define-module (bsp geo plane)
               #:use-module (srfi srfi-1) ;; zip
               #:use-module (srfi srfi-2) ;; and-let*
               #:use-module (srfi srfi-26) ;; cut
               #:use-module (rnrs bytevectors)
               #:use-module (bsp sewer list)
               #:use-module (bsp sewer fn)
               #:use-module (bsp geo consts)
               #:use-module (bsp geo bounds)
               #:use-module (bsp geo vec3)
               #:use-module (bsp geo line)
               #:use-module (bsp geo face)
               #:export (make-plane
                         make-plane-from-points
                         plane-point
                         plane-normal
                         plane-cmp-point
                         plane-cmp-line
                         plane-test-points
                         plane-line-intersection
                         plane-clip-boundary
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
;;;   '= means no intersection, planar
;;;   '+ means no intersection, positive
;;;   '- means no intersection, negative
;;;   'intersection means the plane and poly intersect
;;; This test uses an upper and lower bounding plane. Points that are within the bounding plane are considered 'on the plane'.
;;; This prevents us from clipping planes that are infinitesimally intersected, but would otherwise be unclipped.
;;; If plane-poly intersection is still found, plane-cmp-point should be used with the true plane for an exact sign.
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
             '=]
            [(all? (lambda (upper-sign) (and (or (eq? upper-sign '=) (eq? upper-sign '-)))) upper-signs)
             '-]
            [(all? (lambda (lower-sign) (and (or (eq? lower-sign '=) (eq? lower-sign '+)))) lower-signs)
             '+]
            [else 'intersection]))))

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

;; Returns either a face representing the clipped plane against the boundary or #f
(define (plane-clip-boundary plane boundary)
  ;; points are a list of points on the convex hull of some polygon
  ;; basis-z is the unit normal for the face of the convex poly
  ;; Returns false unless 3+ points are given
  ;; Otherwise sorts the points into a ccw winding
  (define (sort-convex-points points basis-z)
    (cond [(null? points) #f]
          [(null? (cdr points)) #f]
          [(null? (cddr points)) #f]
          [else
            (let* ((origin (car points) )
                   (p0 (cadr points))
                   (basis-x (v3:norm (v3:sub p0 origin)))
                   (basis-y (v3:cross basis-z basis-x))
                   (ranks
                     (map (lambda (pn) (let* ((p (v3:sub pn origin))
                                              (x-component (v3:dot p basis-x))
                                              (y-component (v3:dot p basis-y))
                                              (l (v3:length p))
                                              (rank (if (positive? y-component)
                                                        (acos (/ x-component l))
                                                        (- (* 2 PI) (acos (/ x-component l))))))
                                         (cons rank pn)))
                          (cddr points)))
                   (sorted (sort-list `((,PI . ,origin) . ,ranks) (lambda (a b) (< (car a) (car b))))))
              (cons p0 (map cdr sorted)))]))

  (let* ((corners         (bounds-corners boundary)) ;; list of (point line-index line-index line-index) ...
         (line-candidates (make-bitvector bounds-line-count 1)) ;; Shortcut to ignore lines already covered by corners
         (corner-points
           (filter-map
             (applied (lambda (corner l0 l1 l2)
                        (and (eq? '= (plane-cmp-point plane corner))
                             (begin
                               (bitvector-clear-bit! line-candidates l0)
                               (bitvector-clear-bit! line-candidates l1)
                               (bitvector-clear-bit! line-candidates l2)
                               corner))))
             corners))
         (lines (bounds-lines boundary))
         (points
           (let self ((line-indices (iota (vector-length lines)))
                      (points corner-points))
             (if (null? line-indices)
                 points
                 (let ((point (and-let* ((line-index (car line-indices))
                                         ((bitvector-bit-set? line-candidates line-index))
                                         (point (plane-cmp-line plane (vector-ref lines line-index)))
                                         ((not (eq? point 'none)))
                                         ((not (eq? point 'inf)))
                                         ((within-bounds boundary point)))
                                        point)))
                   (if point
                       (self (cdr line-indices) (cons point points))
                       (self (cdr line-indices) points))))))
         (sorted-points (sort-convex-points points (plane-normal plane))))
    ;; Given set of points that lie on a both a plane and the edges of a bounding square
    ;; Find an ordering for the convex shape formed by the points
    ;; Returns false if the points don't form a polygon
    (and sorted-points
         (make-face sorted-points))))
