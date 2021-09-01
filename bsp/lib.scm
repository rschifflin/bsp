;;; BSP accepts as input a list of 3d points and an index list of polygons, with the normals rising from the 'counter-clockface'
;;; Step 0: Parsing
;;; Should be able to parse the points as a list of 3-element lists
;;; Returns vertices . faces
;;; A vertex is a 3d point
;;; A face is a planar convex polygon composed of vertex indices
(define-module (bsp lib)
               #:use-module (srfi srfi-1)
               #:use-module (ice-9 receive)
               #:use-module (bsp list)
               #:use-module (bsp plane)
               #:use-module (bsp face)
               #:use-module (bsp vec3)
               #:use-module (bsp clip)
               #:use-module (bsp hash)
               #:use-module (bsp tree)
               #:export (make-bsp-tree
                         add-bsp-portals!
                         empty-bsp-tree!
                         face->plane
                         bsp-+leafs
                         bsp-+portals))

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
    (if (null? candidate-faces)
        (make-tree `(,applied-faces ())) ;; Return tree leaf with solids and (empty) portals as data
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

                              ;; Return tree branch with splitting plane data, lhs negative and rhs positive
                              (make-tree splitting-plane
                                         (make-bsp-tree -candidate-faces -applied-faces)
                                         (make-bsp-tree +candidate-faces +applied-faces))))))))
  (make-bsp-tree faces '()))

(define (empty-bsp-tree! tree)
  (if (tree-leaf? tree)
      (tree-set-datum! tree '())
      (for-each empty-bsp-tree! (tree-children tree))))

;; Takes a list of faces, turns them into boundary-clipped polys, then inserts them into the bsp tree in a special way
;; When portal polys lie coincident with a branch plane, they get sent down _both_ ends of the branch with opposite windings, as siblings!
;; When they eventually settle into their respective convex hull, they join the two sibling hulls with the notion of being connected via portal.
;; This means the +hull can "see" the -hull and is used for visibility, collision detection, etc
(define (add-bsp-portals!
          tree            ; BSP-tree
          faces           ; List of convex polygons
          boundary)       ; dimensions which are sufficient to contain all faces
  (define (add-bsp-portal! tree face side)
    (if (tree-leaf? tree)
        ;; Check solids list for duplicate faces
        ;; If none match, add this portal to the portal list

        ;; Only care about leafy portals
        (if (eq? side 'rhs)
            (if (not (find (lambda (other) (face~= face other))
                           (car (tree-datum tree))))
                (let ((new-portal-list (cons face (cadr (tree-datum tree)))))
                  (set-cdr! (tree-datum tree) `(,new-portal-list . ())))))

        ;; Else, not a leaf. Branches have splitting planes as their data
        (let* ((clip (clip-plane-face (tree-datum tree) face)))
          ;; Planar faces are moved to BOTH sets. In the future they'll be married by a common index
          ;; This is because these two 'matching' faces glue two convex hulls together
          (cond [(eq? (car clip) 'clipped)
                 (begin
                   (add-bsp-portal! (tree-rhs tree) (list-ref clip 1) 'rhs)
                   (add-bsp-portal! (tree-lhs tree) (list-ref clip 2) 'lhs))]
                [(eq? (cadr clip) '+)
                 (add-bsp-portal! (tree-rhs tree) face 'rhs)]
                [(eq? (cadr clip) '-)
                 (add-bsp-portal! (tree-lhs tree) face 'lhs)]
                [(eq? (cadr clip) '=)
                 (begin
                   (add-bsp-portal! (tree-rhs tree) face 'rhs)
                   (add-bsp-portal! (tree-lhs tree) face 'lhs))]))))

  (let* ((planes (map face->plane faces))
         (clip (lambda (plane) (clip-plane-boundary plane boundary)))
         (table-len (length planes))
         (hash-plane (lambda (plane size)
                       (let ((h1 (vec3-hash (plane-normal plane) size))
                             (h2 (vec3-hash (plane-normal (plane-flip plane)) size)))
                         (hash (+ h1 h2) size))))
         (unique-planes (dedup-hash planes hash-plane plane~=))
         (bounds-faces (map clip unique-planes)))
    (for-each (lambda (face) (add-bsp-portal! tree face 'rhs)) bounds-faces)))

;; Just the positive leafs of the bsp-tree
(define (bsp-+leafs bsp-tree)
  (let self ((bsp-tree bsp-tree) (leafs '()) (side 'rhs))
    (cond [(null? bsp-tree) leafs]
          [(and (tree-leaf? bsp-tree)
                (eq? side 'lhs)) leafs]
          [(and (tree-leaf? bsp-tree)
                (eq? side 'rhs)) (cons (car (tree-datum bsp-tree)) leafs)]
          [else (self
                  (tree-rhs bsp-tree)
                  (self (tree-lhs bsp-tree) leafs 'lhs)
                  'rhs)])))

(define (bsp-+portals bsp-tree)
  (let self ((bsp-tree bsp-tree) (portals '()) (side 'rhs))
    (cond [(null? bsp-tree) portals]
          [(and (tree-leaf? bsp-tree)
                (eq? side 'lhs)) portals]
          [(and (tree-leaf? bsp-tree)
                (eq? side 'rhs)) (cons (cadr (tree-datum bsp-tree)) portals)]
          [else (self
                  (tree-rhs bsp-tree)
                  (self (tree-lhs bsp-tree) portals 'lhs)
                  'rhs)])))
