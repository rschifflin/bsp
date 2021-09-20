(define-module (bsp serde export)
               #:use-module (srfi srfi-1) ; filter-map
               #:use-module (srfi srfi-43) ; vector ops
               #:use-module (srfi srfi-180) ; Json parsing
               #:use-module (ice-9 receive) ; Multiple-returns
               #:use-module (bsp sewer plist)
               #:use-module (bsp sewer tree)
               #:use-module (bsp sewer display)
               #:use-module (bsp geo vec3)
               #:use-module (bsp geo plane)
               #:use-module (bsp geo poly)
               #:export (export-bsp
                         export-meshes))

;; A mesh is a list of polys
(define (export-meshes meshes port)
  (json-write
    (list->vector (filter-map (lambda (mesh)
                                (receive (verts indexed-polys)
                                         (make-indexed-polys mesh)
                                         (and (positive? (vector-length indexed-polys))
                                              `((vertices . ,verts) (faces . ,indexed-polys)))))
                              meshes))
    port))

;; TODO: Hacky for now
;; Uses internal structure of bsp tree... perhaps an export trait to enforce module boundaries?
(define (export-bsp bsp port)
  ;; Edits a vectree in place to replace all branch splitplanes with index refs to a newly returned vec of splitplanes
  (define (extract-planes! vectree)
    (list->vector
      (reverse (pget (vector-fold (lambda (i state vectree-el)
                                    (if (not (null? (pget vectree-el 'children)))
                                        (begin
                                          (vector-set! vectree i (plist-merge vectree-el (list 'datum (pget state 'index))))
                                          (list 'index (+ 1 (pget state 'index))
                                                'planes (cons (pget vectree-el 'datum) (pget state 'planes))))
                                        state))
                                  (list 'index 0 'planes '())
                                  vectree)
                     'planes))))

  (let* ((vectree (tree->vector (pget bsp 'tree)))
         (planes (extract-planes! vectree))
         (volumes (pget bsp 'vector)))

    ;; Turn a vectree into an alist
    ;; of  (datum . n)
    ;;     (parent . n) if present,
    ;; and (children . [m ...])
    (vector-map! (lambda (i vectree-el)
                   (let ((parent (pget vectree-el 'parent)))
                     (filter identity (list
                       (cons 'datum (pget vectree-el 'datum))
                       (and parent (cons 'parent parent))
                       (cons 'children (list->vector (pget vectree-el 'children)))))))
                 vectree)

    ;; Turn a plane into an alist of (point . #(float float float)) and (normal . #(float float float))
    ;; for json serialization
    (vector-map! (lambda (i plane)
                   (list (cons 'point  (vec3->vector (plane-point plane)))
                         (cons 'normal (vec3->vector (plane-normal plane)))))
                 planes)

    ;; Turn a volume into an alist
    ;; of  (solids . [[#(float float float) ...] ...])
    ;; and (portals . [((face . [#(float float float) ...])
    ;;                  (neighbor . n)) ...])
    ;; for json serialization
    ;; Map non-destructively as volumes come from the source bsp itself
    (let* ((null-volume (list (cons 'solids #()) (cons 'portals #())))
           (volumes
             (vector-map (lambda (i volume)
                           (if (not (pget volume 'inside?))
                               null-volume
                               ;; (solids . [[#(float float float) ...] ...])
                               (list (cons 'solids (list->vector
                                                     (map
                                                       (lambda (poly)
                                                         (list->vector (map vec3->vector (poly-points poly))))
                                                       (pget volume 'solids))))
                                     ;; (portals . [((face . [#(float float float) ...])
                                     ;;              (neighbor . n)) ...])
                                     (cons 'portals (list->vector
                                                      (map
                                                        (lambda (portal)
                                                          (list (cons 'face
                                                                      (list->vector
                                                                        (map vec3->vector
                                                                             (poly-points (pget portal 'face)))))
                                                                (cons 'neighbor
                                                                      (pget portal 'neighbor))))
                                                        (pget volume 'portals)))))))
                         volumes)))

      ;; Final json format
      (json-write (list (cons 'tree vectree)
                        (cons 'planes planes)
                        (cons 'volumes volumes)) port))))

(define (make-indexed-polys mesh)
  (define (make-indexed-poly len.verts points)
    ;; For each point in poly, find matching vertex.
    ;; If no vertex matches, add the point as a new vertex
    ;; Multiple returns: (len . verts) and indexed poly
    (let self ((points points)
               (indexed-poly '())
               (len (car len.verts))
               (verts (cdr len.verts)))
      (if (null? points)
          (values (cons len verts) (list->vector (reverse indexed-poly)))
          (let ((vert-idx (list-index (lambda (vert) (eqv? vert (car points)))
                                     verts)))
            (if (not vert-idx)
                (self (cdr points)
                      (cons len indexed-poly)
                      (+ 1 len)
                      (cons (vec3->vector (car points)) verts))
                (self (cdr points)
                      (cons (- (- len 1) vert-idx) indexed-poly)
                      len
                      verts))))))

  (let self ((mesh mesh) (len.verts '(0)) (indexed-polys '()))
    (if (null? mesh)
        (values (list->vector (reverse (cdr len.verts)))
                (list->vector indexed-polys))
        (receive (new-len.verts indexed-poly)
                 (make-indexed-poly len.verts (poly-points (car mesh)))
                 (self (cdr mesh) new-len.verts (cons indexed-poly indexed-polys))))))
