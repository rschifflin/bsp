(define-module (bsp geo consts)
               #:use-module (bsp geo vec3)
               #:use-module (bsp geo line)
               #:export (PI
                          VECTOR_EPSILON
                          MIN_AREA
                          PLANE_HALFWIDTH
                          FULL_COVERING_RATIO))
(define PI (* 4 (atan 1)))

;;; The difference between two unit vectors
;;; separated by an angle less than 0.000001 degrees is effectively zero.
;;; This means the cross product of two unit vectors is effectively zero past 0.0001deg
;;; This means the dot product of two unit vectors is effectively zero past 89.9999deg
(define VECTOR_EPSILON 1.74532925e-6)

;; Faces smaller than this are degenerate. It may have sides so small that calculations will be grossly inaccurate
(define MIN_AREA 0.000001)

;; For plane split comparisons, we use a slightly-fat plane
;; This prevents degenerate splits and other floating point inaccuracy issues
(define PLANE_HALFWIDTH 0.000001)

;; The amount a face has to be covered to be considered fully covered
(define FULL_COVERING_RATIO 0.999999)
