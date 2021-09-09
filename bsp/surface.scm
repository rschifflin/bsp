;; A surface represents a mesh face with associated materials, UV coords, etc
(define-module (bsp surface)
               #:use-module (bsp sewer tag)
               #:use-module (bsp sewer trait)
               #:use-module (bsp sewer plist)
               #:use-module (bsp geo poly)
               #:export ( make-surface
                          surface-points
                          surface-builder
                          surface-builder-add-point
                          surface-builder-add-points
                          build-surface))

(define-tag <surface> : <poly>)
(define-method (poly-points (surface <surface>)) (surface-points surface))

;; Returns either (#t +split -split intersections) if split,
;; or (#f =|+|-) if no split was required (the surface is either co-planar or lies entirely on the positive or negative side of the plane)
;; Use default helper methods in poly to deref:
;; poly-split?
;; poly-+split
;; poly--split
;; poly-split-intersections
;; poly-non-split-sign
(define-method (poly-split (surface <surface>) plane)
               (define (split-step point,sign props)
                 (let ((+surface (pget props '+surface))
                       (-surface (pget props '-surface))
                       (prev-point (car (pget props 'prev)))
                       (prev-sign (cadr (pget props 'prev)))
                       (intersections (pget props 'intersections))
                       (point (car point,sign))
                       (sign (cadr point,sign)))
                   (cond [(eq? sign '=) ; Planar case; add the idx to both surfaces
                          (let ((+surface (surface-builder-add-point +surface point))
                                (-surface (surface-builder-add-point -surface point)))
                            (plist-merge props (list
                                                 '+surface +surface
                                                 '-surface -surface
                                                 'prev point,sign
                                                 'intersections  (cons point intersections))))]

                         [(and (eq? sign '+)
                               (not (eq? prev-sign '-))) ; Positive non-intersecting case; add idx to positive surface
                          (let ((+surface (surface-builder-add-point +surface point)))
                            (plist-merge props (list
                                                 '+surface +surface
                                                 'prev  point,sign)))]

                         [(and (eq? sign '-)
                               (not (eq? prev-sign '+))) ; Negative non-intersecting case; add idx to negative surface
                          (let ((-surface (surface-builder-add-point -surface point)))
                            (plist-merge props (list
                                                 '-surface -surface
                                                 'prev  point,sign)))]

                         [else ; intersecting case; generate intermediate planar vertex. Add to both surfaces
                           (let* ((line (make-line-from-points prev-point point))
                                  (intersection (plane-line-intersection plane line))
                                  (+surface (if (eq? sign '+)
                                             (surface-builder-add-points +surface `(,intersection ,point))
                                             (surface-builder-add-point +surface intersection)))
                                  (-surface (if (eq? sign '-)
                                             (surface-builder-add-points -surface `(,intersection ,point))
                                             (surface-builder-add-point -surface intersection))))
                             (plist-merge props (list
                                                  '+surface +surface
                                                  '-surface -surface
                                                  'prev  point,sign
                                                  'intersections (cons intersection intersections)
                                                  )))])))
               (let* ((points (surface-points surface))
                      (test (plane-test-points plane points)))
                 (if (eq? test 'intersection)
                     (let* ((signs (map (cut plane-cmp-point plane <>) points))
                            (points,signs (zip points signs))
                            (p0    (car points,signs))
                            (ptail (cdr points,signs))
                            (step0 (list '+surface surface-builder
                                         '-surface surface-builder
                                         'prev p0
                                         'intersections '()))
                            (split-result (split-step p0 (fold split-step step0 ptail)))
                            (+split (build-surface (pget split-result '+surface)))
                            (-split (build-surface (pget split-result '-surface)))
                            (intersections (pget split-result 'intersections)))
                       (cond [(poly-degenerate? +split) (begin
                                                          (println "Split resulted in degenerate surface Resplitting.")
                                                          (poly-split -split plane))]
                             [(poly-degenerate? -split) (begin
                                                         (println "Split resulted in degenerate surface Resplitting.")
                                                         (poly-split +split plane))]
                             [else `(#t ,+split ,-split ,intersections)]))
                     `(#f ,test))))

(define (make-surface surface) (tag surface <surface>))
(define (surface-points surface)
  (pget (untag surface) 'points))
(define surface-builder '())
(define (build-surface builder) (make-surface (list 'points (reverse builder))
                                                    'uvs '()
                                                    'material-id 0))
(define (surface-builder-add-point surfaceb point) (cons point surfaceb))
(define (surface-builder-add-points surfaceb points) (fold cons surfaceb points))
