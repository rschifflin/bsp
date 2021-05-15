(define-module (bsp vec3)
               #:export (@x @y @z
                         make-vec3
                         vec3->vector
                         v3:sum
                         v3:sub
                         v3:scale
                         v3:cross
                         v3:dot
                         v3:norm))
;; Simple vector-3 math
(define (@x v)
  (f32vector-ref v 0))

(define (@y v)
  (f32vector-ref v 1))

(define (@z v)
  (f32vector-ref v 2))

(define make-vec3 f32vector)

(define (vec3->vector v)
  (list->vector `(,(@x v) ,(@y v) ,(@z v))))

(define (v3:sum v0 v1)
  (make-vec3 (+ (@x v0) (@x v1))
           (+ (@y v0) (@y v1))
           (+ (@z v0) (@z v1))))

(define (v3:sub v0 v1)
  (make-vec3 (- (@x v0) (@x v1))
           (- (@y v0) (@y v1))
           (- (@z v0) (@z v1))))

(define (v3:scale v s)
  (make-vec3 (* (@x v) s)
           (* (@y v) s)
           (* (@z v) s)))

(define (v3:cross v0 v1)
  (make-vec3
    (- (* (@y v0) (@z v1))
       (* (@z v0) (@y v1)))
    (- (* (@z v0) (@x v1))
       (* (@x v0) (@z v1)))
    (- (* (@x v0) (@y v1))
       (* (@y v0) (@x v1)))))

(define (v3:dot v0 v1)
  (+ (* (@x v0) (@x v1))
     (* (@y v0) (@y v1))
     (* (@z v0) (@z v1))))

(define (v3:norm v)
  (let* ((x (@x v))
         (y (@y v))
         (z (@z v))
         (len (sqrt (+ (* x x)
                       (* y y)
                       (* z z)))))
    (make-vec3
      (/ x len)
      (/ y len)
      (/ z len))))

