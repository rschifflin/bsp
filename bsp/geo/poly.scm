;; Trait for working w/ convex, planar shapes
(define-module (bsp geo poly)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-26)
               #:use-module (bsp sewer list)
               #:use-module (bsp sewer trait)
               #:use-module (bsp geo consts)
               #:use-module (bsp geo plane)
               #:use-module (bsp geo line)
               #:use-module (bsp geo vec3)
               #:export (<poly>
                          poly-points
                          poly-split

                          ;; Default methods
                          poly-area
                          poly-planar?
                          poly-degenerate?
                          poly-normal
                          poly-intersection
                          poly-carve
                          poly-carve-all
                          poly->plane

                          ;; Helper split ADT
                          poly-split?
                          poly-+split
                          poly--split
                          poly-split-intersections
                          poly-non-split-sign))

;; interface to implement
(define-trait <poly> (;; poly-points poly
                      ;; Returns a list of vec3 points
                      (poly-points (self <self>))

                      ;; poly-split poly plane
                      ;; If the plane splits the poly...
                      ;;   returns (#t +split -split intersections)
                      ;; Else
                      ;;   returns (#f =|+|-) where the sign determines where the non-split poly sits in relation to the plane
                      (poly-split (self <self>) plane)))

(define poly-split? car)

;; Only true splits have a +poly, -poly, and intersections
(define (poly-+split split)
  (list-ref split 1))

(define (poly--split split)
  (list-ref split 2))

(define (poly-split-intersections split)
  (list-ref split 3))

(define (poly-non-split-sign split)
  (list-ref split 1))

(define-method (poly-area (poly <poly>))
               (let* ((points (poly-points poly))
                      (p0 (first points))
                      (tris (zip (cdr points) (cddr points))))
                 (let self ((tris tris) (area 0))
                   (if (null? tris)
                       area
                       (let* ((tri (car tris))
                              (p1 (first tri))
                              (p2 (second tri)))
                         (self (cdr tris) (+ area
                                             (/ (v3:length (v3:cross (v3:sub p1 p0)
                                                                     (v3:sub p2 p0)))
                                                2))))))))

;; A poly is planar if all its tris share the same normal
(define-method (poly-planar? (poly <poly>))
  (define (tri->normal tri)
    (let* ((p0 (list-ref tri 0))
           (p1 (list-ref tri 1))
           (p2 (list-ref tri 2)))
      (v3:norm (v3:cross (v3:sub p1 p0)
                         (v3:sub p2 p0)))))
  (let* ((points (poly-points poly))
         (plane  (poly->plane poly))
         (normals (map tri->normal (zip points
                                        (cdr points)
                                        (cddr points)))))
    (all? (cut v3:norm= <> (car normals)) normals)))

(define-method (poly-degenerate? (poly <poly>)) (< (poly-area poly) MIN_AREA))

(define-method (poly->plane (poly <poly>))
  (let* ((points (poly-points poly))
         (p0 (list-ref points 0))
         (p1 (list-ref points 1))
         (p2 (list-ref points 2)))
    (make-plane-from-points p0 p1 p2)))

(define-method (poly-normal (poly <poly>))
  (let* ((points (poly-points poly))
         (p0 (list-ref points 0))
         (p1 (list-ref points 1))
         (p2 (list-ref points 2)))
    (v3:norm (v3:cross (v3:sub p1 p0) (v3:sub p2 p0)))))

;; Returns a poly representing the intersection between co-planar polys a and b, or #f if they do not intersect
(define-method (poly-intersection (a <poly>) (b <poly>))
  (let* ((normal (poly-normal a))
         (points (poly-points a))
         (plast (last points))
         (lines (zip (cons plast points) points)))
    (if (not (v3:norm= normal (poly-normal b)))
        (error "poly-intersection error: arguments a and b must be co-planar to intersect")
        (let self ((lines lines) (poly b))
          (if (null? lines) (and (not (poly-degenerate? poly)) poly)
              (let* ((endpoints (car lines))
                     (p0 (first endpoints))
                     (p1 (second endpoints))
                     (tangent (v3:norm (v3:cross normal (v3:sub p1 p0))))
                     (plane (make-plane p0 tangent))
                     (split (poly-split poly plane)))
                (cond [(poly-split? split)
                       (self (cdr lines) (poly-+split split))]
                      [(eq? (poly-non-split-sign split) '+)
                       (self (cdr lines) poly)]
                      [(or (eq? (poly-non-split-sign split) '-)
                           (eq? (poly-non-split-sign split) '=)) #f] ;; If the entire remaining poly lies on the negative side of a line,
                                                     ;; the line represents a separating plane and thus no intersection is possible.
                                                     ;; We can immediately return false
                      )))))))

;; Returns a list of polys formed from b by carving a out of the overlapping area of b.
;; If there is no overlap, returns b whole
;; The result covers the same area as a + b, but without overlap
(define-method (poly-carve (a <poly>) (b <poly>))
  (let* ((normal (poly-normal a))
         (points (poly-points a))
         (plast (last points))
         (lines (zip (cons plast points) points)))
    (if (not (v3:norm= normal (poly-normal b)))
        (error "poly-carve error: arguments a and b must be co-planar to carve")
        (let self ((lines lines) (poly b) (new-polys '()))
          (if (null? lines) (filter (lambda (poly) (not (poly-degenerate? poly))) new-polys)
              (let* ((endpoints (car lines))
                     (p0 (first endpoints))
                     (p1 (second endpoints))
                     (tangent (v3:norm (v3:cross normal (v3:sub p1 p0))))
                     (plane (make-plane p0 tangent))
                     (split (poly-split poly plane)))
                (cond [(poly-split? split)
                       (if (line-segments-overlap? endpoints (poly-split-intersections split))
                           (self (cdr lines) (poly-+split split) (cons (poly--split split) new-polys))
                           (self (cdr lines) poly new-polys))]
                      [(eq? (poly-non-split-sign split) '+)
                       (self (cdr lines) poly new-polys)]
                      [(or (eq? (poly-non-split-sign split) '-)
                           (eq? (poly-non-split-sign split) '=))
                       (self '() poly (cons poly new-polys))])))))))

;; Given a list of polys, return a list of non-overlapping carved polys
(define-method (poly-carve-all polys)
  (fold (lambda (poly carves) ;; For each poly we want to carve...
          (append carves ;; Append the poly pieces we carve up into the carvelist, to further carve next polys
                  (let self ((subpolys `(,poly)) (carves carves))
                    (if (null? carves)
                        subpolys
                        (flat-map (lambda (subpoly)
                                    (self (poly-carve (car carves) subpoly)
                                          (cdr carves)))
                                  subpolys)))))
        '()
        polys))
