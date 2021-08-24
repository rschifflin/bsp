(define-module (bsp bounds)
               #:use-module (bsp vec3)
               #:use-module (bsp line)
               #:use-module (bsp line)
               #:export (make-boundary
                         within-bounds
                         bounds-lines
                         bounds-corners
                         bounds-line-count))

  ;; Boundary cube is defined by 12 lines (1-12) defining 6 cubefaces (A-F)
  ;; Face A is the +Z face ("front")
  ;; Face B is the +X face ("right")
  ;; Face C is the -Z face ("back")
  ;; Face D is the -X face ("left")
  ;; Face E is the +Y face ("top")
  ;; Face F is the -Y face ("bottom")

  ;; -- L8 --
  ;; |      |
  ;; L11 E  L5
  ;; |      |
  ;; -- L1 --- L5 --- L8 --- L11--
  ;; |      |      |      |      |
  ;; L2  A  L3  B  L7  C  L10 D  L2
  ;; |      |      |      |      |
  ;; -- L4 --- L6 --- L9 --- L12--
  ;; |      |
  ;; L12 F  L6
  ;; |      |
  ;; -- L9 --

  ;; Connectivity:

  ;; LINEMAP
  ;; Line|Faces
  ;;   01| A E
  ;;   02| A D
  ;;   03| A B
  ;;   04| A F
  ;;   05| B E
  ;;   06| B F
  ;;   07| B C
  ;;   08| C E
  ;;   09| C F
  ;;   10| C D
  ;;   11| D E
  ;;   12| D F

  ;; FACEMAP
  ;; Face| Lines
  ;;    A| 01 02 03 04
  ;;    B| 05 03 07 06
  ;;    C| 08 07 10 09
  ;;    D| 11 10 12 02
  ;;    E| 08 11 05 01
  ;;    F| 04 12 06 09

  ;; CORNERMAP
  ;; Corner|
  ;;      0| ABE
  ;;      1| ADE
  ;;      2| ABF
  ;;      3| BCE
  ;;      4| ADF
  ;;      5| BCF
  ;;      6| CDE
  ;;      7| CDF


(define-syntax alias
  (syntax-rules ()
    ((alias ((sym . val)))
     (define sym val))

    ((alias (pair pair* ...))
     (begin
       (alias (pair))
       (alias (pair* ...))))))

(alias (
    ;; Faces
    (A . 0)
    (B . 1)
    (C . 2)
    (D . 3)
    (E . 4)
    (F . 5)

    ; Lines
    (L01 . 0)
    (L02 . 1)
    (L03 . 2)
    (L04 . 3)
    (L05 . 4)
    (L06 . 5)
    (L07 . 6)
    (L08 . 7)
    (L09 . 8)
    (L10 . 9)
    (L11 . 10)
    (L12 . 11)))

(define make-boundary identity)
(define bounds-line-count 12)
(define (within-bounds boundary point)
  (and
    (>= (@x point) (- boundary))
    (<= (@x point) boundary)

    (>= (@y point) (- boundary))
    (<= (@y point) boundary)

    (>= (@z point) (- boundary))
    (<= (@z point) boundary)))

(define (bounds-corners boundary)
  (let* ((b boundary)
         (-b (- boundary))
         (+x+y+z (make-vec3  b  b  b))
         (-x+y+z (make-vec3 -b  b  b))
         (+x-y+z (make-vec3  b -b  b))
         (+x+y-z (make-vec3  b  b -b))
         (-x-y+z (make-vec3 -b -b  b))
         (+x-y-z (make-vec3  b -b -b))
         (-x+y-z (make-vec3 -b  b -b))
         (-x-y-z (make-vec3 -b -b -b)))

    (list
      (list +x+y+z L01 L03 L05)
      (list -x+y+z L01 L02 L11)
      (list +x-y+z L03 L04 L06)
      (list +x+y-z L05 L07 L08)
      (list -x-y+z L02 L04 L12)
      (list +x-y-z L06 L07 L09)
      (list -x+y-z L08 L10 L11)
      (list -x-y-z L09 L10 L12))))

(define (bounds-lines boundary)
  (let ((b boundary)
        (-b (- boundary)))
    (vector
      (make-line ; L01
        (make-vec3 0.0 b b)
        (make-vec3 1.0 0.0 0.0))

      (make-line ; L02
        (make-vec3 -b 0.0 b)
        (make-vec3 0.0 1.0 0.0))

      (make-line ; L03
        (make-vec3 b 0.0 b)
        (make-vec3 0.0 1.0 0.0))

      (make-line ; L04
        (make-vec3 0.0 -b b)
        (make-vec3 1.0 0.0 0.0))

      (make-line ; L05
        (make-vec3 b b 0.0)
        (make-vec3 0.0 0.0 1.0))

      (make-line ; L06
        (make-vec3 b -b 0.0)
        (make-vec3 0.0 0.0 1.0))

      (make-line ; L07
        (make-vec3 b 0.0 -b)
        (make-vec3 0.0 1.0 0.0))

      (make-line ; L08
        (make-vec3 0.0 b -b)
        (make-vec3 1.0 0.0 0.0))

      (make-line ; L09
        (make-vec3 0.0 -b -b)
        (make-vec3 1.0 0.0 0.0))

      (make-line ; L10
        (make-vec3 -b 0.0 -b)
        (make-vec3 0.0 1.0 0.0))

      (make-line ; L11
        (make-vec3 -b b 0.0)
        (make-vec3 0.0 0.0 1.0))

      (make-line ; L12
        (make-vec3 -b -b 0.0)
        (make-vec3 0.0 0.0 1.0)))))
