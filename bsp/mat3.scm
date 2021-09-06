;; NOTE: All mat3 operations are column-major
;; Ie the f32vector is represented sequentially as
;; [col0row0, col0row1, col0row2, col1row0, col1row1,col1row2, col2row0, col2row1, col2row2]

(define-module (bsp mat3)
               #:use-module (bsp vec3)
               #:export (@m
                         make-mat3
                         m3:v3:*
                         m3:rotate-align))

(define (@m mat3 col row)
  (f64vector-ref mat3 (+ (* 3 col) row)))

(define make-mat3 f64vector)

(define (m3:v3:* m v)
  (make-vec3
    (+ (* (@m m 0 0) (@x v))
       (* (@m m 0 1) (@y v))
       (* (@m m 0 2) (@z v)))

    (+ (* (@m m 1 0) (@x v))
       (* (@m m 1 1) (@y v))
       (* (@m m 1 2) (@z v)))

    (+ (* (@m m 2 0) (@x v))
       (* (@m m 2 1) (@y v))
       (* (@m m 2 2) (@z v)))))

;; Returns the mat3 rotation matrix such that (dot (* rot-mat vsource) vdest) == 1
;; IE the matrix which aligns vsource to vdest
;; See https://gist.github.com/kevinmoran/b45980723e53edeb8a5a43c49f134724
(define (m3:rotate-align vsource vdest)
  (let* ((axis (v3:cross vdest vsource))
         (cos-a (v3:dot vsource vdest))

         (k (if (and
                  (> cos-a (- 1.000000000001))
                  (< cos-a (- 0.999999999999)))
                0.0
                (/ 1.0 (+ 1.0 cos-a)))))

    (make-mat3
      (+ (* (@x axis) (@x axis) k) cos-a)     ; Col0 Row0
      (+ (* (@y axis) (@x axis) k) (@z axis)) ; Col0 Row1
      (- (* (@z axis) (@x axis) k) (@y axis)) ; Col0 Row2

      (- (* (@x axis) (@y axis) k) (@z axis)) ; Col1 Row0
      (+ (* (@y axis) (@y axis) k) cos-a)     ; Col1 Row1
      (+ (* (@z axis) (@y axis) k) (@x axis)) ; Col1 Row2

      (+ (* (@x axis) (@z axis) k) (@y axis)) ; Col2 Row0
      (- (* (@y axis) (@z axis) k) (@x axis)) ; Col2 Row1
      (+ (* (@z axis) (@z axis) k) cos-a))))  ; Col2 Row2
