(define-module (bsp serde export)
               #:use-module (srfi srfi-1) ; filter-map
               #:use-module (srfi srfi-180) ; Json parsing
               #:use-module (ice-9 receive) ; Multiple-returns
               #:use-module (bsp geo poly)
               #:use-module (bsp geo vec3)
               #:export (export-meshes))

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
