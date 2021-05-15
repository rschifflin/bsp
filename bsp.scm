(add-to-load-path (getcwd))
(use-modules
  (srfi srfi-1) ; list-index
  (srfi srfi-43) ; Vector map
  (srfi srfi-180) ; Json parsing
  (ice-9 receive) ; Multiple-returns
  (bsp alist)
  (bsp vec3)
  (bsp lib))

(define json-in (call-with-input-file "concave.json" json-read))

;; Convert vertices/faces to fat face list
(display "Starting import...")
(newline)

(define poly (vector-ref json-in 0))
(define faces (vector->list
                (vector-map (lambda (_index face)
                              (vector->list (vector-map (lambda (_index vert-idx)
                                                          (let* ((points (vector-ref (aref poly 'vertices) vert-idx))
                                                                 (p0 (vector-ref points 0))
                                                                 (p1 (vector-ref points 1))
                                                                 (p2 (vector-ref points 2)))
                                                            (make-vec3 p0 p1 p2)))
                                                        face)))
                            (aref poly 'faces))))

(display "Import complete.")
(newline)

(display "Starting tree-ify")
(newline)

(define tree (make-bsp-tree faces))
(define leafs (bsp-+leafs tree))

(display "Bsp tree complete")
(newline)

(define (make-indexed-faces leaf)
  (define (make-indexed-face len.verts face)
    ;; For each point in face, find matching vertex.
    ;; If no vertex matches, add the point as a new vertex
    ;; Multiple returns: (len . verts) and indexed face
    (let self ((face face)
               (indexed-face '())
               (len (car len.verts))
               (verts (cdr len.verts)))
      (if (null? face)
          (values (cons len verts) (list->vector (reverse indexed-face)))
          (let ((vert-idx (list-index (lambda (vert) (eqv? vert (car face)))
                                     verts)))
            (if (not vert-idx)
                (self (cdr face)
                      (cons len indexed-face)
                      (+ 1 len)
                      (cons (vec3->vector (car face)) verts))
                (self (cdr face)
                      (cons (- (- len 1) vert-idx) indexed-face)
                      len
                      verts))))))

  (let self ((leaf leaf) (len.verts '(0)) (faces '()))
    (if (null? leaf)
        (values (list->vector (reverse (cdr len.verts)))
                (list->vector faces))
        (receive (new-len.verts face)
                 (make-indexed-face len.verts (car leaf))
                 (self (cdr leaf) new-len.verts (cons face faces))))))

(display "Starting export...")
(newline)

(define convex-out (list->vector (map (lambda (leaf)
                                        (receive (verts faces)
                                                 (make-indexed-faces leaf)
                                                 `((vertices . ,verts) (faces . ,faces))))
                                      leafs)))

(call-with-output-file "convex.json" (lambda (port) (json-write convex-out port)))

(display "Export complete")
(newline)
