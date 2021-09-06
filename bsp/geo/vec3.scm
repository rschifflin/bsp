(define-module (bsp geo vec3)
               #:export (@x @y @z
                         make-vec3
                         vec3->vector
                         vec3->u8vector
                         vec3-hash
                         v3:=
                         v3:~=
                         v3:<
                         v3:~<
                         v3:ord
                         v3:sum
                         v3:sub
                         v3:neg
                         v3:scale
                         v3:cross
                         v3:dot
                         v3:norm
                         v3:length))
;; Simple vector-3 math
(define (@x v)
  (f64vector-ref v 0))

(define (@y v)
  (f64vector-ref v 1))

(define (@z v)
  (f64vector-ref v 2))

(define make-vec3 f64vector)

;; Partially orders two vec3s.
;; Returns a two-element list, (smaller, larger)
(define (v3:ord cmp? v0 v1)
  (let self ((idx 0))
    (if (= idx 3)
        `(,v0 ,v1)
        (let ((a (f64vector-ref v0 idx))
              (b (f64vector-ref v1 idx)))
          (cond [(= a b) (self (+ 1 idx))]
                [(cmp? a b) `(,v0 ,v1)]
                [else `(,v1 ,v0)])))))

(define (vec3->vector v)
  (list->vector `(,(@x v) ,(@y v) ,(@z v))))

(define vec3->u8vector identity)

(define (vec3-hash v size)
  (remainder
    (+ (* 5 (hash (@x v) size))
       (* 3 (hash (@y v) size))
       (hash (@z v) size))
    size))

(define (v3:= v0 v1)
  (and (= (@x v0) (@x v1))
       (= (@y v0) (@y v1))
       (= (@z v0) (@z v1))))

(define EPSILON 0.0001)

(define (~= x y)
  (and (< (- x y) EPSILON)
       (> (- x y) (- EPSILON))))

(define (v3:~= v0 v1)
  (and (~= (@x v0) (@x v1))
       (~= (@y v0) (@y v1))
       (~= (@z v0) (@z v1))))

(define (v3:neg v)
  (make-vec3 (- (@x v))
             (- (@y v))
             (- (@z v))))

(define (v3:< v0 v1)
  (if (= (@x v0) (@x v1))
      (if (= (@y v0) (@y v1))
          (< (@z v0) (@z v1))
          (< (@y v0) (@y v1)))
      (< (@x v0) (@x v1))))

(define (v3:~< v0 v1)
  (if (~= (@x v0) (@x v1))
      (if (~= (@y v0) (@y v1))
          (and (not (~= (@z v0) (@z v1)))
               (< (@z v0) (@z v1)))
          (< (@y v0) (@y v1)))
      (< (@x v0) (@x v1))))

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

(define (v3:length v)
  (let ((x (@x v))
        (y (@y v))
        (z (@z v)))
    (sqrt (+ (* x x)
             (* y y)
             (* z z)))))

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
