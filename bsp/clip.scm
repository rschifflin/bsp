(define-module (bsp clip)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-2)
               #:use-module (bsp fn)
               #:use-module (bsp macros)
               #:use-module (bsp list)
               #:use-module (bsp alist)
               #:use-module (bsp vector)
               #:use-module (bsp bounds)
               #:use-module (bsp plane)
               #:use-module (bsp line)
               #:use-module (bsp vec3)
               #:use-module (bsp mat3)
               #:export (clip-plane-boundary
                         clip-plane-face
                         sort-convex-points))

(define PI (* 4 (atan 1)))

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
                      (self (cdr line-indices) points))))))

    ;; Given set of points that lie on a both a plane and the edges of a bounding square
    ;; Find an ordering for the convex shape formed by the points
    ;; Returns false if the points don't form a polygon
    (sort-convex-points points (plane-normal plane))))

;;; Clip a face by a plane
;;; Returns ADT, one of:
;;;   ('clipped +face -face)
;;;   ('unclipped <+|-|=>)
(define (clip-plane-face plane face)
  (let* ((test-result (plane-test-face plane face))
         (needs-clip? (car test-result)))

    ;; clip props are an alist
    ;; '(+face . (indices...))
    ;; '(-face . (indices...))
    ;; '(prev . (point sign))
    (define (clip-point point,sign props)
      (let ((+face (aref props '+face))
            (-face (aref props '-face))
            (prev-point (car (aref props 'prev)))
            (prev-sign (cadr (aref props 'prev)))
            (point (car point,sign))
            (sign (cadr point,sign)))

        (cond [(eq? sign '=) ; Planar case; add the idx to both faces
               (let ((+face (cons point +face))
                     (-face (cons point -face)))
                 (amerge props `((+face . ,+face)
                                 (-face . ,-face)
                                 (prev . ,point,sign))))]

              [(and (eq? sign '+)
                    (not (eq? prev-sign '-))) ; Positive non-intersecting case; add idx to positive face
               (let ((+face (cons point +face)))
                    (amerge props `((+face . ,+face)
                                    (prev . ,point,sign))))]

              [(and (eq? sign '-)
                    (not (eq? prev-sign '+))) ; Negative non-intersecting case; add idx to negative face
               (let ((-face (cons point -face)))
                    (amerge props `((-face . ,-face)
                                    (prev . ,point,sign))))]

              [else ; intersecting case; generate intermediate planar vertex. Add to both faces
                (let* ((line (make-line-from-points prev-point point))
                       (vert (plane-line-intersection plane line))
                       (+face (if (eq? sign '+)
                                  (cons point (cons vert +face))
                                  (cons vert +face)))
                       (-face (if (eq? sign '-)
                                  (cons point (cons vert -face))
                                  (cons vert -face))))
                  (amerge props `((+face . ,+face)
                                  (-face . ,-face)
                                  (prev . ,point,sign))))])))

    (define (clip signs)
      (let* ((face+signs (zip face signs))
             (p0    (car face+signs))
             (ptail (cdr face+signs))
             (props `((+face . ())
                      (-face . ())
                      (prev . ,p0)))
             (props (clip-point p0 (fold clip-point props ptail)))
             (+face (reverse (aref props '+face)))
             (-face (reverse (aref props '-face))))
          `(,+face ,-face)))

    (if needs-clip?
      `(clipped . ,(clip (cadr test-result)))
      `(unclipped ,(cadr test-result)))))

#| TODO: Testing

(let* ((clip-result (clip-plane-face plane face))
       (clipped? (eq? 'clipped (car clip-result))))

  (if clipped?
      (let* ((+face (list-ref clip-result 1))
             (-face (list-ref clip-result 2)))
        (display "CLIPPED")
        (newline)
        (display +face)
        (newline)
        (display -face))

      ;; Else, unclipped
      (let ((sign (cadr clip-result)))
        (cond [(eq? sign '=) (display "PLANAR, NOTHING TO CLIP")]
              [(eq? sign '+) (display "POSITIVE, NOTHING TO CLIP")]
              [(eq? sign '-) (display "NEGATIVE, NOTHING TO CLIP")])))
        (newline))
|#

