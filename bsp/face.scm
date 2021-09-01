;;; Defines the interface of a face; a 2d convex polygon
;;; Represented internally as a list of points ordered in a ccw winding
(define-module (bsp face)
               #:use-module (srfi srfi-1)
               #:use-module (bsp vec3)
               #:export (make-face
                          face=
                          face~=
                          face-builder
                          face-builder-add-point
                          face-builder-add-points
                          build-face))

(define make-face identity)
(define face-builder '())
(define (face-builder-add-point faceb point) (cons point faceb))
(define (face-builder-add-points faceb points) (fold cons faceb points))

(define (face= f0 f1)
  (and (= (length f0) (length f1))
       (let ((f0-sorted (sort f0 v3:<))
             (f1-sorted (sort f1 v3:<)))
         (let self ((lst0 f0-sorted) (lst1 f1-sorted))
           (cond [(null? lst0) #t]
                 [(not (v3:= (car lst0) (car lst1))) #f]
                 [else (self (cdr lst0) (cdr lst1))])))))

(define (face~= f0 f1)
  (and (= (length f0) (length f1))
       (let ((f0-sorted (sort f0 v3:~<))
             (f1-sorted (sort f1 v3:~<)))
         (let self ((lst0 f0-sorted) (lst1 f1-sorted))
           (cond [(null? lst0) #t]
                 [(not (v3:~= (car lst0) (car lst1))) #f]
                 [else (self (cdr lst0) (cdr lst1))])))))

(define build-face reverse)
