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
               #:use-module (bsp plist)
               #:use-module (bsp tree)
               #:export (make-bsp
                         add-bsp-portals!
                         mark-inside!
                         bsp-leafs
                         bsp-solids
                         bsp-portals
                         bsp-+solids
                         bsp-+portals))

;;; Choose from a working-set a polygon-face to define the splitting plane
(define (choose-splitting-face working-set)
  (car working-set))

;;; Derive a plane from the face
;;; Since the face is planar, sampling any 3 points is enough to define the plane.
(define (partition-splits splitting-plane faces,clips)

  ;;; Returns four values, +face, -face, +=face, -=face
  ;;; += and -= indicate the face is planar, but aligned with the plane normal in +=, and opposite in -=
  (define (partition-split face,clip)
    (let ((face (car face,clip))
          (clip (cadr face,clip)))
      (cond [(clip-clipped? clip) (values (clip-+face clip) (clip--face clip) #f #f)]
            ;; Otherwise unclipped
            [(eq? '+ (clip-sign clip)) (values face #f #f #f)]
            [(eq? '- (clip-sign clip)) (values #f face #f #f)]
            [(eq? '= (clip-sign clip))
             ;; Determine +/- value based on normal alignment
             (if (> (v3:dot (plane-normal splitting-plane) (plane-normal (face->plane face)))
                    0)
                 (values #f #f face #f)
                 (values #f #f #f face))])))

  (let self ((faces,clips faces,clips) (+splits '()) (-splits '()) (+=splits '()) (-=splits '()))
    (if (null? faces,clips)
        (values +splits -splits +=splits -=splits)
        (receive (+split -split +=split -=split)
                 (partition-split (car faces,clips))
                 (self (cdr faces,clips)
                       (if +split (cons +split +splits) +splits)
                       (if -split (cons -split -splits) -splits)
                       (if +=split (cons +=split +=splits) +=splits)
                       (if -=split (cons -=split -=splits) -=splits))))))

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

          ;; Partition splits into +/-
          (receive (+candidates -candidates +=candidates -=candidates)
                   (partition-splits splitting-plane (zip candidate-faces candidate-clips))
                   (receive (+applied -applied +=applied -=applied)
                            (partition-splits splitting-plane (zip applied-faces applied-clips))
                            ;; Planar candidates are moved to the applied set to prevent re-splitting along the same plane
                            ;; Planar faces that share the splitting normal are pushed right
                            ;; Planar faces with opposite normals are pushed left
                            ;; thus making a 'right-handed' leafy bsp tree
                            (let* ((+candidate-faces +candidates)
                                   (-candidate-faces -candidates)
                                   (+applied-faces (append +applied +=applied +=candidates))
                                   (-applied-faces (append -applied -=applied -=candidates))
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
    (list 'lhs (fill-neighborhood lhs bounds-face '())
          'rhs (fill-neighborhood rhs bounds-face '()))))

(define (fill-neighborhood bsp-tree bounds-face neighbors)
  (if (tree-leaf? bsp-tree)
      ;; If it's a leaf
      (let* ((index (tree-datum bsp-tree)))
        (cons (list 'index index 'face bounds-face) neighbors))

      ;; Else, not a leaf. Branches have splitting planes as their data
      (let* ((splitplane (tree-datum bsp-tree))
             (clip (clip-plane-face splitplane bounds-face)))
        (cond [(clip-clipped? clip)
               (fill-neighborhood (tree-rhs bsp-tree) (clip-+face clip)
                 (fill-neighborhood (tree-lhs bsp-tree) (clip--face clip) neighbors))]
              [(eq? (clip-sign clip) '+)
               (fill-neighborhood (tree-rhs bsp-tree) bounds-face neighbors)]
              [(eq? (clip-sign clip) '-)
               (fill-neighborhood (tree-lhs bsp-tree) bounds-face neighbors)]

              ;; If ANOTHER coincident splitplane is present while we're already generating a neighborhood, we've broken an invariant
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
               (let ((neighborhood (gen-neighborhood bsp-tree bounds-face)))
                 (cons neighborhood neighborhoods))]))))

;; Return a list of paired neighbors
;; TODO: Currently n^2 in complexity as every lhs node checks every rhs node
;; Can be sped up by sorting faces along an abritrary planar axis, and have every lhs node binary-search rhs nodes
(define (neighborhood-pair-neighbors neighborhood pairings)
  ;; Returns a neighbor-pair or #f if the two aren't neighbors
  (define (make-neighbor-pair left-neighbor right-neighbor)
    (let* ((left-index (pget left-neighbor 'index))
           (left-face (pget left-neighbor 'face))
           (right-index (pget right-neighbor 'index))
           (right-face (pget right-neighbor 'face))
           (intersection (face:intersection left-face right-face)))
      (and intersection
           (list 'indices `(,left-index ,right-index)
                 'face    intersection))))

  (define (pairs-for-neighbor neighbor partners pairings)
    (if (null? partners)
        pairings
        (let* ((partner (car partners))
               (neighbor-pair (make-neighbor-pair neighbor partner)))
          (if neighbor-pair
              (pairs-for-neighbor neighbor (cdr partners) (cons neighbor-pair pairings))
              (pairs-for-neighbor neighbor (cdr partners) pairings)))))

  (let ((lhs (pget neighborhood 'lhs))
        (rhs (pget neighborhood 'rhs)))
    (if (or (null? lhs)
            (null? rhs))
        pairings
        (fold (lambda (left-neighbor pairings)
                (pairs-for-neighbor left-neighbor rhs pairings))
              pairings
              lhs))
    ))

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
             (neighbor-pairings (fold neighborhood-pair-neighbors '() neighborhoods))
             (pairing-lhs-index (lambda (pairing) (first (pget pairing 'indices))))
             (pairing-rhs-index (lambda (pairing) (second (pget pairing 'indices)))))

        ;; Given the list of neighbor pairings, and index fns to choose
        ;; which side to write to and which side to connect to,
        ;; - Groups up all the neighbors by their leaf index.
        ;; - For each group of neighbors that share a leaf index...
        ;; -- Calculates the set of non-overlapping solids that might cover the portal
        ;; -- Iterates through each neighbor, writing the portal to the leaf unless blocked by the covering set
        (define (set-neighbors! pairings index-fn dest-fn)
          (let* ((sorted (sort pairings (lambda (pairing other)
                                                   (< (index-fn pairing)
                                                      (index-fn other)))))
                 (index-runs (runs (lambda (pairing other)
                                     (= (index-fn pairing)
                                        (index-fn other))) sorted)))
            (for-each (lambda (run) ;; Each run is guaranteed to be non-empty
                        (let* ((leaf-index (index-fn (car run)))
                               (leaf-data (vector-ref bsp-vector leaf-index))
                               (plane (face->plane face))
                               (covering-set (pget leaf-data 'solids))
                               (covering-set (filter (lambda (solid) (plane~= plane (face->plane solid)))
                                                     covering-set))
                               (covering-set (face:carve-all covering-set)))
                          (for-each (lambda (pairing)
                                      (let* ((dest-index (dest-fn pairing))
                                             (portal-face (pget pairing 'face))
                                             (portal-area (face:area portal-face))
                                             (portal-covering-set (filter-map (lambda (face)
                                                                                (face:intersection portal-face face))
                                                                              covering-set))
                                             (covering-area (apply + (map face:area portal-covering-set)))
                                             (portals (pget leaf-data 'portals))
                                             (new-portal (list 'face portal-face 'neighbor dest-index))
                                             (new-leaf-data (pput leaf-data 'portals (cons new-portal portals))))

                                        ;; TODO: Measure by some epsilon. Be aware of accumulated rounding errors!
                                        (if (> portal-area (+ 0.00001 covering-area))
                                            (vector-set! bsp-vector leaf-index new-leaf-data))))
                                    run)))
                      index-runs)))

        ;; Write the lhs portals, pointing to the rhs neighbors
        (set-neighbors! neighbor-pairings pairing-lhs-index pairing-rhs-index)

        ;; Write the rhs portals, pointing to the lhs neighbors
        (set-neighbors! neighbor-pairings pairing-rhs-index pairing-lhs-index)))
    (for-each (lambda (face) (add-bsp-portal! bsp-tree face)) bounds-faces)))

(define (mark-inside! bsp point-inside)
  (define (find-inside-leaf bsp-tree point-inside)
    (cond [(null? bsp-tree) #f]
          [(tree-leaf? bsp-tree)
           (tree-datum bsp-tree)]
          [else
            (let ((splitting-plane (tree-datum bsp-tree)))
              (if (negative? (v3:dot (v3:sub point-inside (plane-point splitting-plane))
                                     (plane-normal splitting-plane)))
                  (find-inside-leaf (tree-lhs bsp-tree) point-inside)
                  (find-inside-leaf (tree-rhs bsp-tree) point-inside)))]))
  (let* ((bsp-tree (plist-get bsp 'tree))
         (bsp-vector (plist-get bsp 'vector))
         (leaf-index (find-inside-leaf bsp-tree point-inside)))
    (let self ((leaf-index leaf-index) (depth-limit 256))
      (let ((leaf-data (vector-ref bsp-vector leaf-index)))
        (if (and (> depth-limit 0)
                 (not (plist-ref leaf-data 'inside?)))
            (begin
              (display "MARKING LEAF: ")
              (display leaf-index)
              (newline)
              (vector-set! bsp-vector
                           leaf-index
                           (plist-put leaf-data 'inside? #t))
              (for-each (lambda (portal)
                          (self (plist-get portal 'neighbor) (- depth-limit 1)))
                        (plist-ref leaf-data 'portals))))))))



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

;; All leafs of the bsp-tree
(define (bsp-leafs bsp)
  (let ((bsp-tree   (plist-get bsp 'tree))
        (bsp-vector (plist-get bsp 'vector)))
    (let self ((bsp-tree bsp-tree) (leafs '()))
      (cond [(null? bsp-tree) leafs]
            [(tree-leaf? bsp-tree)
             (let* ((index (tree-datum bsp-tree))
                    (leaf (vector-ref bsp-vector index)))
               (cons leaf leafs))]
            [else (self
                    (tree-rhs bsp-tree)
                    (self (tree-lhs bsp-tree) leafs))]))))

;; All properties of a leaf of the bsp tree
(define (bsp-prop bsp prop)
  (let ((bsp-tree   (plist-get bsp 'tree))
        (bsp-vector (plist-get bsp 'vector)))
    (let self ((bsp-tree bsp-tree) (leafs '()))
      (cond [(null? bsp-tree) leafs]
            [(tree-leaf? bsp-tree)
             (let* ((index (tree-datum bsp-tree))
                    (leaf (vector-ref bsp-vector index)))
               (cons (plist-ref leaf prop)
                     leafs))]
            [else (self
                    (tree-rhs bsp-tree)
                    (self (tree-lhs bsp-tree) leafs))]))))


(define (bsp-+solids bsp)
  (bsp-+prop bsp 'solids))

(define (bsp-+portals bsp)
  (bsp-+prop bsp 'portals))

(define (bsp-solids bsp)
  (bsp-prop bsp 'solids))

(define (bsp-portals bsp)
  (bsp-prop bsp 'portals))
