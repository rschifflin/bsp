(define-module (bsp serde export)
               #:use-module (srfi srfi-1) ; filter-map
               #:use-module (srfi srfi-180) ; Json parsing
               #:use-module (ice-9 receive) ; Multiple-returns
               #:use-module (bsp geo face)
               #:use-module (bsp geo vec3)
               #:export (export-meshes))

;; A mesh is a list of faces
(define (export-meshes meshes port)
  (json-write
    (list->vector (filter-map (lambda (mesh)
                                (receive (verts indexed-faces)
                                         (make-indexed-faces mesh)
                                         (and (positive? (vector-length indexed-faces))
                                              `((vertices . ,verts) (faces . ,indexed-faces)))))
                              meshes))
    port))

(define (make-indexed-faces mesh)
  (define (make-indexed-face len.verts points)
    ;; For each point in face, find matching vertex.
    ;; If no vertex matches, add the point as a new vertex
    ;; Multiple returns: (len . verts) and indexed face
    (let self ((points points)
               (indexed-face '())
               (len (car len.verts))
               (verts (cdr len.verts)))
      (if (null? points)
          (values (cons len verts) (list->vector (reverse indexed-face)))
          (let ((vert-idx (list-index (lambda (vert) (eqv? vert (car points)))
                                     verts)))
            (if (not vert-idx)
                (self (cdr points)
                      (cons len indexed-face)
                      (+ 1 len)
                      (cons (vec3->vector (car points)) verts))
                (self (cdr points)
                      (cons (- (- len 1) vert-idx) indexed-face)
                      len
                      verts))))))

  (let self ((mesh mesh) (len.verts '(0)) (indexed-faces '()))
    (if (null? mesh)
        (values (list->vector (reverse (cdr len.verts)))
                (list->vector indexed-faces))
        (receive (new-len.verts indexed-face)
                 (make-indexed-face len.verts (face-points (car mesh)))
                 (self (cdr mesh) new-len.verts (cons indexed-face indexed-faces))))))
