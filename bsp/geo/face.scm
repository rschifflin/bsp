;;; Defines the interface of a face; a 2d convex planar polygon
;;; Represented internally as a list of points ordered in a ccw winding
(define-module (bsp geo face)
               #:use-module (srfi srfi-1)
               #:use-module (bsp sewer list)
               #:use-module (bsp geo plane)
               #:use-module (bsp geo vec3)
               #:use-module (bsp clip)
               #:export (make-face
                          face:area
                          face:intersection
                          face:carve
                          face:carve-all
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


;; TODO: Where should this helper function live? in (bsp line)? (bsp line-segment)?
;; Helper function to determine if two co-linear segments defined by the lists (a0 a1) and (b0 b1), where ax and bx are vec3 points,
;; overlap or not.
(define (line-segments-overlap? segment1 segment2)
  (let* ((a0 (first segment1))
         (a1 (second segment1))
         (bi (first segment2))  ;; The ordering of b0 and b1 are unclear
         (bj (second segment2)) ;; Label i and j initially until ordering is known

         ;; If the segment2 vector points opposite the segment1 vector, flip it
         ;; This orders segment2 such that the vector b1-b0 points the same direction as the vector a1-a0
         (segment2 (if (< (v3:dot (v3:sub a1 a0)
                                  (v3:sub bj bi))
                               0)
                            (reverse segment2)
                            segment2))
         (b0 (first segment2))
         (b1 (second segment2)))

    ;; If any of the following conditions hold, the line segments must overlap:
    (or (v3:~= b0 a0)                                   ;; b0 ~= a0
        (v3:~= b1 a1)                                   ;; b1 ~= a1
        (> (v3:dot (v3:sub b0 a0) (v3:sub a1 b0)) 0)    ;; b0 between a0 and a1
        (> (v3:dot (v3:sub b1 a0) (v3:sub a1 b1)) 0)    ;; b1 between a0 and a1
        (> (v3:dot (v3:sub a0 b0) (v3:sub b1 a0)) 0)    ;; a0 between b0 and b1
        (> (v3:dot (v3:sub a1 b0) (v3:sub b1 a1)) 0)))) ;; a1 between b0 and b1

(define (face:area face)
  (let* ((points (face-points face))
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

;; Returns a face representing the intersection between co-planar faces f1 and f2, or #f if they do not intersect
(define (face:intersection f1 f2)
  (let* ((normal (face-normal f1))
         (points (face-points f1))
         (plast (last points))
         (lines (zip (cons plast points) points)))
    (if (not (or (v3:~= normal (face-normal f2))
                 (v3:~= normal (v3:neg (face-normal f2)))))
        (error "face:intersection error: arguments f1 and f2 must be co-planar to intersect")
        (let self ((lines lines) (face f2))
          (if (null? lines) face
              (let* ((endpoints (car lines))
                     (p0 (first endpoints))
                     (p1 (second endpoints))
                     (tangent (v3:norm (v3:cross normal (v3:sub p1 p0))))
                     (plane (make-plane p0 tangent))
                     (clip (clip-plane-face plane face)))
                (cond [(clip-clipped? clip)
                       (self (cdr lines) (clip-+face clip)) ]
                      [(eq? (clip-sign clip) '+)
                       (self (cdr lines) face)]
                      [(eq? (clip-sign clip) '-) #f] ;; If the entire remaining face lies on the negative side of a line,
                                                     ;; the line represents a separating plane and thus no intersection is possible.
                                                     ;; We can immediately return false
                      )))))))

;; Returns a list of convex faces formed from f2 by carving f1 out of the overlapping area of f2.
;; If there is no overlap, returns f2 whole
;; The result covers the same area as f1 + f2, but without overlap
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
                     (p0 (first endpoints))
                     (p1 (second endpoints))
                     (tangent (v3:norm (v3:cross normal (v3:sub p1 p0))))
                     (plane (make-plane p0 tangent))
                     (clip (clip-plane-face plane face)))
                (cond [(clip-clipped? clip)
                       (if (line-segments-overlap? endpoints (clip-intersections clip))
                           (self (cdr lines) (clip-+face clip) (cons (clip--face clip) new-faces))
                           (self (cdr lines) face new-faces))]
                      [(eq? (clip-sign clip) '+)
                       (self (cdr lines) face new-faces)]
                      [(eq? (clip-sign clip) '-)
                       (self '() face (cons face new-faces))])))))))

;; Given a list of faces, return a list of non-overlapping carved faces
(define (face:carve-all faces)
  (fold (lambda (face carves) ;; For each face we want to carve...
          (append carves ;; Append the face pieces we carve up into the carvelist, to further carve next faces
                  (let self ((subfaces `(,face)) (carves carves))
                    (if (null? carves)
                        subfaces
                        (flat-map (lambda (subface)
                                    (self (face:carve (car carves) subface)
                                          (cdr carves)))
                                  subfaces)))))
        '()
        faces))

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
