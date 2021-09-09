;;; Defines the interface of a face; a 2d convex planar polygon
;;; Represented internally as a list of points ordered in a ccw winding
(define-module (bsp geo face)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-26)
               #:use-module (bsp sewer tag)
               #:use-module (bsp sewer trait)
               #:use-module (bsp sewer list)
               #:use-module (bsp sewer plist)
               #:use-module (bsp sewer display)
               #:use-module (bsp geo consts)
               #:use-module (bsp geo plane)
               #:use-module (bsp geo line)
               #:use-module (bsp geo vec3)
               #:use-module (bsp geo poly)
               #:export ( make-face
                          face-points
                          face-builder
                          face-builder-add-point
                          face-builder-add-points
                          build-face))

(define-tag <face> : <poly>)
(define-method (poly-points (face <face>)) (face-points face))

;; Returns either (#t +split -split intersections) if split,
;; or (#f =|+|-) if no split was required (the face is either co-planar or lies entirely on the positive or negative side of the plane)
;; Use default helper methods in poly to deref:
;; poly-split?
;; poly-+split
;; poly--split
;; poly-split-intersections
;; poly-non-split-sign
(define-method (poly-split (face <face>) plane)
               (define (split-step point,sign props)
                 (let ((+face (pget props '+face))
                       (-face (pget props '-face))
                       (prev-point (car (pget props 'prev)))
                       (prev-sign (cadr (pget props 'prev)))
                       (intersections (pget props 'intersections))
                       (point (car point,sign))
                       (sign (cadr point,sign)))
                   (cond [(eq? sign '=) ; Planar case; add the idx to both faces
                          (let ((+face (face-builder-add-point +face point))
                                (-face (face-builder-add-point -face point)))
                            (plist-merge props (list
                                                 '+face +face
                                                 '-face -face
                                                 'prev point,sign
                                                 'intersections  (cons point intersections))))]

                         [(and (eq? sign '+)
                               (not (eq? prev-sign '-))) ; Positive non-intersecting case; add idx to positive face
                          (let ((+face (face-builder-add-point +face point)))
                            (plist-merge props (list
                                                 '+face +face
                                                 'prev  point,sign)))]

                         [(and (eq? sign '-)
                               (not (eq? prev-sign '+))) ; Negative non-intersecting case; add idx to negative face
                          (let ((-face (face-builder-add-point -face point)))
                            (plist-merge props (list
                                                 '-face -face
                                                 'prev  point,sign)))]

                         [else ; intersecting case; generate intermediate planar vertex. Add to both faces
                           (let* ((line (make-line-from-points prev-point point))
                                  (intersection (plane-line-intersection plane line))
                                  (+face (if (eq? sign '+)
                                             (face-builder-add-points +face `(,intersection ,point))
                                             (face-builder-add-point +face intersection)))
                                  (-face (if (eq? sign '-)
                                             (face-builder-add-points -face `(,intersection ,point))
                                             (face-builder-add-point -face intersection))))
                             (plist-merge props (list
                                                  '+face +face
                                                  '-face -face
                                                  'prev  point,sign
                                                  'intersections (cons intersection intersections)
                                                  )))])))
               (let* ((points (face-points face))
                      (test (plane-test-points plane points)))
                 (if (eq? test 'intersection)
                     (let* ((signs (map (cut plane-cmp-point plane <>) points))
                            (points,signs (zip points signs))
                            (p0    (car points,signs))
                            (ptail (cdr points,signs))
                            (step0 (list '+face face-builder
                                         '-face face-builder
                                         'prev p0
                                         'intersections '()))
                            (split-result (split-step p0 (fold split-step step0 ptail)))
                            (+split (build-face (pget split-result '+face)))
                            (-split (build-face (pget split-result '-face)))
                            (intersections (pget split-result 'intersections)))
                       (cond [(poly-degenerate? +split) (begin
                                                          (println "Split resulted in degenerate face. Resplitting.")
                                                          (poly-split -split plane))]
                             [(poly-degenerate? -split) (begin
                                                          (println "Split resulted in degenerate face. Resplitting.")
                                                          (poly-split +split plane))]
                             [else `(#t ,+split ,-split ,intersections)]))
                     `(#f ,test))))

(define (make-face points) (tag points <face>))
(define face-points untag)

(define face-builder '())
(define (build-face builder) (make-face (reverse builder)))
(define (face-builder-add-point faceb point) (cons point faceb))
(define (face-builder-add-points faceb points) (fold cons faceb points))
