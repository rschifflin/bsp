;;; BSP accepts as input a list of 3d points and an index list of polygons, with the normals rising from the 'counter-clockface'
;;; Step 0: Parsing
;;; Should be able to parse the points as a list of 3-element lists
;;; Returns vertices . faces
;;; A vertex is a 3d point
;;; A face is a planar convex polygon composed of vertex indices

(define-module (bsp lib)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 receive)
               #:use-module (bsp plane)
               #:use-module (bsp clip)
               #:export (make-bsp-tree
                         bsp-+leafs))

;;; Choose from a working-set a polygon-face to define the splitting plane
(define (choose-splitting-face working-set)
  (car working-set))

;;; Derive a plane from the face
;;; Since the face is planar, sampling any 3 points is enough to define the plane.
(define (face->plane face)
  (let* ((p0 (list-ref face 0))
         (p1 (list-ref face 1))
         (p2 (list-ref face 2)))
    (make-plane-from-points p0 p1 p2)))


(define (partition-splits faces,clips)
  ;;; Returns three values, +face, -face, =face
  (define (partition-split face,clip)
    (let ((face (car face,clip))
          (clip (cadr face,clip)))

      (cond [(eq? 'clipped (car clip)) (values (list-ref clip 1) (list-ref clip 2) '())]
            ;; Otherwise unclipped
            [(eq? '+ (list-ref clip 1)) (values face '() '())]
            [(eq? '- (list-ref clip 1)) (values '() face '())]
            [(eq? '= (list-ref clip 1)) (values '() '() face)])))

  (let self ((faces,clips faces,clips) (+splits '()) (-splits '()) (=splits '()))
    (if (null? faces,clips)
        (values +splits -splits =splits)
        (receive (+split -split =split)
                 (partition-split (car faces,clips))
                 (self (cdr faces,clips)
                       (if (null? +split) +splits (cons +split +splits))
                       (if (null? -split) -splits (cons -split -splits))
                       (if (null? =split) =splits (cons =split =splits)))))))


(define (make-bsp-tree faces)
  (define (make-bsp-tree candidate-faces applied-faces)
    (if (null? candidate-faces) `(leaf ,applied-faces)
      (let* ((splitting-face  (choose-splitting-face candidate-faces))
             (splitting-plane (face->plane splitting-face))
             (clip (lambda (face) (clip-plane-face splitting-plane face)))
             (candidate-clips (map clip candidate-faces))
             (applied-clips (map clip applied-faces)))

        ;; Partition splits into +/-/=
        (receive (+candidates -candidates =candidates)
                 (partition-splits (zip candidate-faces candidate-clips))
                 (receive (+applied -applied =applied)
                          (partition-splits (zip applied-faces applied-clips))

                          ;; Planar candidates are moved to the applied set to prevent re-splitting along the same plane
                          ;; We choose planar faces to be positive, thus making a 'right-handed' leafy bsp tree
                          (let ((+candidate-faces +candidates)
                                (-candidate-faces -candidates)
                                (+applied-faces (append +applied =applied =candidates))
                                (-applied-faces -applied))


                            ;; Return tree with splitting plane, lhs negative and rhs positive
                            `(node
                               ,splitting-plane
                               ,(make-bsp-tree -candidate-faces -applied-faces)
                               ,(make-bsp-tree +candidate-faces +applied-faces))))))))
  (make-bsp-tree faces '()))

;; Just the positive leafs of the bsp-tree
(define (bsp-+leafs bsp-tree)
  (let self ((bsp-tree bsp-tree) (leafs '()) (side 'rhs))
    (cond [(null? bsp-tree) leafs]
          [(and (eq? (car bsp-tree) 'leaf)
                (eq? side 'lhs)) leafs]
          [(and (eq? (car bsp-tree) 'leaf)
                (eq? side 'rhs)) (cons (cadr bsp-tree) leafs)]
          [(eq? (car bsp-tree) 'node)
                (self
                  (list-ref bsp-tree 3)
                  (self (list-ref bsp-tree 2) leafs 'lhs)
                  'rhs)])))
