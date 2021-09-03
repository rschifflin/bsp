(define-module (bsp clip)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-2)
               #:use-module (bsp fn)
               #:use-module (bsp macros)
               #:use-module (bsp list)
               #:use-module (bsp plist)
               #:use-module (bsp bounds)
               #:use-module (bsp plane)
               #:use-module (bsp line)
               #:use-module (bsp face)
               #:use-module (bsp vec3)
               #:use-module (bsp mat3)
               #:export (clip-plane-boundary
                         clip-plane-face
                         clip-clipped?
                         clip-+face
                         clip--face
                         clip-sign
                         clip-intersections
                         sort-convex-points))

(define PI (* 4 (atan 1)))

;; TODO: Move this into libface
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

;; Returns either a face representing the clipped plane against the boundary or #f
(define (clip-plane-boundary plane boundary)
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
                 (if* (point ? (and-let* ((line-index (car line-indices))
                                          ((bitvector-bit-set? line-candidates line-index))
                                          (point (plane-cmp-line plane (vector-ref lines line-index)))
                                          ((not (eq? point 'none)))
                                          ((not (eq? point 'inf)))
                                          ((within-bounds boundary point)))
                                         point))
                      (self (cdr line-indices) (cons point points))
                      (self (cdr line-indices) points)))))
          (sorted-points (sort-convex-points points (plane-normal plane))))
    ;; Given set of points that lie on a both a plane and the edges of a bounding square
    ;; Find an ordering for the convex shape formed by the points
    ;; Returns false if the points don't form a polygon
    (and sorted-points
         (make-face sorted-points))))


(define (clip-clipped? clip)
  (eq? 'clipped (car clip)))

;; Only clipped have a +face, -face, and intersections
(define (clip-+face clip)
  (list-ref clip 1))

(define (clip--face clip)
  (list-ref clip 2))

(define (clip-intersections clip)
  (list-ref clip 3))

;; Only unclipped have a sign
(define (clip-sign clip)
  (list-ref clip 1))

;;; TODO: Add parametric clip points to allow follow-up clipping of UV coordinates
;;; Clip a face by a plane
;;; Returns clip ADT, one of:
;;;   ('clipped +face -face)
;;;   ('unclipped <+|-|=>)
(define (clip-plane-face plane face)
  (let ((test (plane-test-face plane face)))

    ;; clip props are a plist
    ;; '+face face-builder '-face face-builder 'prev (point sign)
    (define (clip-point point,sign props)
      (let ((+face (plist-ref props '+face))
            (-face (plist-ref props '-face))
            (prev-point (car (plist-ref props 'prev)))
            (prev-sign (cadr (plist-ref props 'prev)))
            (intersections (plist-ref props 'intersections))
            (point (car point,sign))
            (sign (cadr point,sign)))

        (cond [(eq? sign '=) ; Planar case; add the idx to both faces
               (let ((+face (face-builder-add-point +face point))
                     (-face (face-builder-add-point -face point)))
                 (plist-merge props (list
                                      '+face +face
                                      '-face -face
                                      'prev  point,sign)))]

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

    (define (clip signs)
      (let* ((face+signs (zip face signs))
             (p0    (car face+signs))
             (ptail (cdr face+signs))
             (props (list '+face face-builder
                          '-face face-builder
                          'prev p0
                          'intersections '()))
             (props (clip-point p0 (fold clip-point props ptail)))
             (+face (build-face (plist-ref props '+face)))
             (-face (build-face (plist-ref props '-face)))
             (intersections (plist-ref props 'intersections)))
          `(,+face ,-face ,intersections)))

    (if (plane-test-intersects? test)
      `(clipped . ,(clip (plane-test-sign test)))
      `(unclipped ,(plane-test-signs test)))))
