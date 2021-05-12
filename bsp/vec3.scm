(define-module (bsp vec3)
               #:export (@x @y @z
                         make-vec3
                         v3:sum
                         v3:sub
                         v3:scale
                         v3:cross
                         v3:dot
                         v3:norm))
;; Simple vector-3 math
(define (@x v)
  (list-ref v 0))

(define (@y v)
  (list-ref v 1))

(define (@z v)
  (list-ref v 2))

(define (make-vec3 x y z)
  `(,x ,y ,z))

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
  (make-vec3 (- (* (@x v0) (@z v1))
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

