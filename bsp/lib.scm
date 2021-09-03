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
               #:use-module (bsp plist)
               #:use-module (bsp tree)
               #:export (make-bsp
                         add-bsp-portals!
                         bsp-+solids
                         bsp-+portals))

;;; Choose from a working-set a polygon-face to define the splitting plane
(define (choose-splitting-face working-set)
  (car working-set))

;;; Derive a plane from the face
;;; Since the face is planar, sampling any 3 points is enough to define the plane.
(define (partition-splits faces,clips)
  ;;; Returns three values, +face, -face, =face
  (define (partition-split face,clip)
    (let ((face (car face,clip))
          (clip (cadr face,clip)))

      (cond [(clip-clipped? clip) (values (clip-+face clip) (clip--face clip) '())]
            ;; Otherwise unclipped
            [(eq? '+ (clip-sign clip)) (values face '() '())]
            [(eq? '- (clip-sign clip)) (values '() face '())]
            [(eq? '= (clip-sign clip)) (values '() '() face)])))

  (let self ((faces,clips faces,clips) (+splits '()) (-splits '()) (=splits '()))
    (if (null? faces,clips)
        (values +splits -splits =splits)
        (receive (+split -split =split)
                 (partition-split (car faces,clips))
                 (self (cdr faces,clips)
                       (if (null? +split) +splits (cons +split +splits))
                       (if (null? -split) -splits (cons -split -splits))
                       (if (null? =split) =splits (cons =split =splits)))))))

;; A bsp combines a spatial lookup tree and an indexed lookup vector
(define (make-bsp faces)
  (define (make-bsp-tree    ;; Returns a pair of size,tree where branches represent splitplanes, and leaves hold indices into a vector of leaf data
            candidate-faces ;; List of faces which haven't been chosen as splitplanes yet
            applied-faces   ;; List of faces already chosen as splitplanes before
            index           ;; Counter representing existing number of leaves
            leaf-list)       ;; List holding leaf data in index order
    (if (null? candidate-faces)
        (list 'index (+ index 1)
              'tree (make-tree index)
              'list (cons (list 'solids applied-faces 'portals '())
                          leaf-list))
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
                            (let* ((+candidate-faces +candidates)
                                   (-candidate-faces -candidates)
                                   (+applied-faces (append +applied =applied =candidates))
                                   (-applied-faces -applied)
                                   (tree-lhs (make-bsp-tree -candidate-faces -applied-faces index leaf-list))
                                   (tree-rhs (make-bsp-tree +candidate-faces +applied-faces (pget tree-lhs 'index) (pget tree-lhs 'list))))

                              ;; Return size.tree branch with splitting plane data, lhs negative and rhs positive
                              (plist-put tree-rhs 'tree (make-tree splitting-plane
                                                                   (pget tree-lhs 'tree)
                                                                   (pget tree-rhs 'tree)))))))))

  (if (null? faces)
      '()
      (let ((bsp-tree (make-bsp-tree faces '() 0 '())))
        (list 'tree (pget bsp-tree 'tree)
              'vector (list->vector (reverse (pget bsp-tree 'list)))
              'faces faces))))

;; Takes the bsp, turns each unique faceplane into a boundary-clipped poly, then inserts each into the bsp tree in a special way
;; When portal polys lie coincident with a branch plane, they get sent down _both_ ends of the branch as siblings!
;; When they eventually settle into their respective convex hull, they join the two sibling hulls with the notion of being connected via portal.
;; This gives a notion of adjacency; the +hull is adjacent to the -hull

;; A neighborhood is a list of all subdivisions of a bounds-face, with their respective index, separated into the lhs siblings and rhs siblings.

;; Portal lifecycle
;; Step 1: Get split into siblings and subdivided along a coincident splitting plane, forming a neighborhood
;; Step 2: Further subdivide into intersections with all neighbors of the opposite side
;; Step 3: Eliminate remaining subdivisions that are completely covered by solids
;; Step 4: Write remaining subdivisions as portals between two neighbors
(define (gen-neighborhood bsp-tree bounds-face)
  (let ((lhs (tree-lhs bsp-tree))
        (rhs (tree-rhs bsp-tree)))
    (list 'lhs (fill-neighborhood lhs bounds-face 'lhs '())
          'rhs (fill-neighborhood rhs bounds-face 'rhs '()))))

(define (fill-neighborhood bsp-tree bounds-face side neighbors)
  (if (tree-leaf? bsp-tree)
      ;; If it's a leaf
      (let* ((index (tree-datum bsp-tree)))
        ;; Only care about 'leafy' convex spaces on the rhs
        (if (eq? side 'rhs)
            (cons (list 'index index 'face bounds-face) neighbors)
            neighbors))

      ;; Else, not a leaf. Branches have splitting planes as their data
      (let* ((splitplane (tree-datum bsp-tree))
             (clip (clip-plane-face splitplane bounds-face)))
        (cond [(clip-clipped? clip)
               (fill-neighborhood (tree-rhs bsp-tree) (clip-+face clip) 'rhs
                 (fill-neighborhood (tree-lhs bsp-tree) (clip--face clip) 'lhs neighbors))]
              [(eq? (clip-sign clip) '+)
               (fill-neighborhood (tree-rhs bsp-tree) bounds-face 'rhs neighbors)]
              [(eq? (clip-sign clip) '-)
               (fill-neighborhood (tree-lhs bsp-tree) bounds-face 'lhs neighbors)]

              ;; If ANOTHER splitplane is present while we're already generating a neighborhood, we've broken an invariant
              ;; Once we split by a given plane, it should never appear again as a splitplane
              [(eq? (clip-sign clip) '=)
               (error "Invariant broken: Identical splitplane found multiple times in bsp path")]))))

(define (find-neighborhoods bsp-tree bounds-face neighborhoods)
  (if (tree-leaf? bsp-tree)
      ;; Leaves are dead-ends in the neighborhood search
      neighborhoods

      ;; Else, not a leaf. Branches have splitting planes as their data
      (let* ((splitplane (tree-datum bsp-tree))
             (clip (clip-plane-face splitplane bounds-face)))
        (cond [(clip-clipped? clip)
               (find-neighborhoods (tree-rhs bsp-tree) (clip-+face clip)
                 (find-neighborhoods (tree-lhs bsp-tree) (clip--face clip) neighborhoods))]
              [(eq? (clip-sign clip) '+)
               (find-neighborhoods (tree-rhs bsp-tree) bounds-face neighborhoods)]
              [(eq? (clip-sign clip) '-)
               (find-neighborhoods (tree-lhs bsp-tree) bounds-face neighborhoods)]

              ;; Neighborhood candidate found
              [(eq? (clip-sign clip) '=)
               (cons (gen-neighborhood bsp-tree bounds-face) neighborhoods)]))))

(define (add-bsp-portals!
          bsp             ; BSP
          boundary)       ; dimensions which are sufficient to contain all faces

  (let* ((bsp-tree   (pget bsp 'tree))
         (bsp-vector (pget bsp 'vector))
         (faces      (pget bsp 'faces))
         (planes (map face->plane faces))
         (hash-plane (lambda (plane size)
                       (let ((h1 (vec3-hash (plane-normal plane) size))
                             (h2 (vec3-hash (plane-normal (plane-flip plane)) size)))
                         (hash (+ h1 h2) size))))
         (unique-planes (dedup-hash planes hash-plane plane~=))
         (clip (lambda (plane) (or (clip-plane-boundary plane boundary)
                                   (error "Could not clip plane to boundary."
                                          "Plane: " plane
                                          "Boundary: " boundary))))
         (bounds-faces (map clip unique-planes)))

    (define (add-bsp-portal! bsp-tree face)
      (let* ((neighborhoods (find-neighborhoods bsp-tree face '()))
             (set-neighbor! (lambda (neighbor)
                              (let* ((index (pget neighbor 'index))
                                     (portal (pget neighbor 'face))
                                     (leaf-data (vector-ref bsp-vector index))
                                     (solids (pget leaf-data 'solids))
                                     (portals (pget leaf-data 'portals))
                                     (new-leaf-data (pput leaf-data 'portals (cons portal portals))))
                                (if (not (find (lambda (other) (face~= portal other)) solids))
                                    (vector-set! bsp-vector index new-leaf-data)))))
             (set-neighbors! (lambda (neighborhood)
                               (for-each set-neighbor! (pget neighborhood 'lhs))
                               (for-each set-neighbor! (pget neighborhood 'rhs)))))
        (for-each set-neighbors! neighborhoods)))
    (for-each (lambda (face) (add-bsp-portal! bsp-tree face)) bounds-faces)))

;; Just the positive leafs of the bsp-tree
(define (bsp-+prop bsp prop)
  (let ((bsp-tree   (plist-get bsp 'tree))
        (bsp-vector (plist-get bsp 'vector)))
    (let self ((bsp-tree bsp-tree) (leafs '()) (side 'rhs))
      (cond [(null? bsp-tree) leafs]
            [(and (tree-leaf? bsp-tree)
                  (eq? side 'lhs))
             leafs]
            [(and (tree-leaf? bsp-tree)
                  (eq? side 'rhs))
             (let* ((index (tree-datum bsp-tree))
                    (leaf (vector-ref bsp-vector index)))
               (cons (plist-ref leaf prop)
                     leafs))]
            [else (self
                    (tree-rhs bsp-tree)
                    (self (tree-lhs bsp-tree) leafs 'lhs)
                    'rhs)]))))

(define (bsp-+solids bsp)
  (bsp-+prop bsp 'solids))

(define (bsp-+portals bsp)
  (bsp-+prop bsp 'portals))
