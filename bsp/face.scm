;;; Defines the interface of a face; a 2d convex planar polygon
;;; Represented internally as a list of points ordered in a ccw winding
(define-module (bsp face)
               #:use-module (srfi srfi-1)
               #:use-module (bsp list)
               #:use-module (bsp vec3)
               #:use-module (bsp plane)
               #:use-module (bsp clip)
               #:export (make-face
                          face:carve
                          face=
                          face~=
                          face-builder
                          face-builder-add-point
                          face-builder-add-points
                          build-face
                          face->plane))

(define make-face identity)
(define face-builder '())
(define (face-builder-add-point faceb point) (cons point faceb))
(define (face-builder-add-points faceb points) (fold cons faceb points))

;; TODO:
;; Returns a face representing the intersection between co-planar faces f1 and f2, or #f if they do not intersect
(define (face:intersection f1 f2)
  #f)

;; Returns a list of convex faces formed from f2 by carving f1 out of the overlapping area of f2.
;; If not overlap,
;; The result covers the same area as f1 + f2, but with no overlapping
(define (face:carve f1 f2)
  (let* ((normal (face-normal f1))
         (points (face-points f1))
         (plast (last points))
         (lines (zip (cons plast points) points)))
    (if (not (or (v3:~= normal (face-normal f2))
                 (v3:~= normal (v3:neg (face-normal f2)))))
        (error "face:carve error: arguments f1 and f2 must be co-planar to carve")
        (let self ((lines lines) (face f2) (new-faces '()))
          (if (null? lines) new-faces
              (let* ((endpoints (car lines))
                     (p0 (car endpoints))
                     (p1 (cadr endpoints))
                     (tangent (v3:norm (v3:cross normal (v3:sub p1 p0))))
                     (plane (make-plane p0 tangent))
                     (clip (clip-plane-face plane face)))

                (cond [(clip-clipped? clip)
                       (let* ((intersections (clip-intersections clip))
                              (i0 (car intersections))
                              (i1 (cadr intersections))
                              ;; If the intersection vector points opposite the line vector, flip it
                              (intersections (if (< (v3:dot (v3:sub p1 p0)
                                                            (v3:sub i1 i0))
                                                    0)
                                                 (reverse intersections)
                                                 intersections))
                              (i0 (car intersections))
                              (i1 (cadr intersections)))
                         (if (or (v3:~= i0 p0)
                                 (v3:~= i1 p1)
                                 (> (v3:dot (v3:sub i0 p0) (v3:sub p1 i0)) 0) ;; i0 between p0 and p1
                                 (> (v3:dot (v3:sub i1 p0) (v3:sub p1 i1)) 0) ;; i1 between p0 and p1
                                 (>  (v3:dot (v3:sub p0 i0) (v3:sub i1 p0)) 0) ;; p0 between i0 and i1
                                 (>  (v3:dot (v3:sub p1 i0) (v3:sub i1 p1)) 0)) ;; p1 between i0 and i1
                             ;; Then the line segment intersected f2. The +clip may still be clipped by further lines. The -clip can be set aside as a new face
                             (self (cdr lines) (clip-+face clip) (cons (clip--face clip) new-faces))
                             ;; Else the line segment did not intersect f2
                             (self (cdr lines) face new-faces)))]
                      [(eq? (clip-sign clip) '+)
                       (self (cdr lines) face new-faces)]
                      [(eq? (clip-sign clip) '-)
                       (self '() face (cons face new-faces))])))))))

(define (face= f0 f1)
  (and (= (length f0) (length f1))
       (let ((f0-sorted (sort f0 v3:<))
             (f1-sorted (sort f1 v3:<)))
         (let self ((lst0 f0-sorted) (lst1 f1-sorted))
           (cond [(null? lst0) #t]
                 [(not (v3:= (car lst0) (car lst1))) #f]
                 [else (self (cdr lst0) (cdr lst1))])))))

(define (face~= f0 f1)
  (and (= (length f0) (length f1))
       (let ((f0-sorted (sort f0 v3:~<))
             (f1-sorted (sort f1 v3:~<)))
         (let self ((lst0 f0-sorted) (lst1 f1-sorted))
           (cond [(null? lst0) #t]
                 [(not (v3:~= (car lst0) (car lst1))) #f]
                 [else (self (cdr lst0) (cdr lst1))])))))

(define (face-normal face)
  (let ((p0 (list-ref face 0))
        (p1 (list-ref face 1))
        (p2 (list-ref face 2)))
    (v3:norm (v3:cross (v3:sub p1 p0) (v3:sub p2 p0)))))

(define face-points identity)

(define build-face reverse)
(define (face->plane face)
  (let* ((p0 (list-ref face 0))
         (p1 (list-ref face 1))
         (p2 (list-ref face 2)))
    (make-plane-from-points p0 p1 p2)))
