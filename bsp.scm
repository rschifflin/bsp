(add-to-load-path (getcwd))
(use-modules
  (srfi srfi-1) ; list-index
  (srfi srfi-43) ; Vector map
  (srfi srfi-180) ; Json parsing
  (ice-9 receive) ; Multiple-returns
  (bsp list)
  (bsp plist)
  (bsp bounds)
  (bsp alist)
  (bsp vec3)
  (bsp lib))

(define json-in (call-with-input-file "concave.json" json-read))
(define (println x)
  (display x) (newline))

;; Convert vertices/faces to fat face list
(println "Starting import...")

(define poly (vector-ref json-in 0))
(define faces (vector->list
                (vector-map (lambda (_index face)
                              (vector->list (vector-map (lambda (_index vert-idx)
                                                          (let* ((vertex (vector-ref (aref poly 'vertices) vert-idx))
                                                                 (x (vector-ref vertex 0))
                                                                 (y (vector-ref vertex 1))
                                                                 (z (vector-ref vertex 2)))
                                                            (make-vec3 x y z)))
                                                        face)))
                            (aref poly 'faces))))

(println "Import complete.")

(println "Sanitizing faces...")

;; TODO: Hacky. Do this properly in a make-face constructor. Doesn't account for future face features like UV coords
(define (sanitize-face face)
  (define (sanitize-edges edges v0)
    (if (null? edges) '()
        (let* ((endpoints (car edges))
               (p0 (car endpoints))
               (p1 (cadr endpoints))
               (v1 (v3:sub p1 p0)))
          (cond [(v3:~= p0 p1)
                 (sanitize-edges (cdr edges) v0)]
                [(and v0 (= 0 (v3:length (v3:cross v0 v1))))
                 (sanitize-edges (cdr edges) v0)]
                [else
                  (cons p0 (sanitize-edges (cdr edges) v1))]
                ))))
  (let* ((edges (zip (cons (last face) face) face))
         (sanitized (sanitize-edges edges #f)))
    (if (< (length sanitized) 3)
        (error "could not save face: " face)
        sanitized)))

(define faces (map sanitize-face faces))


(println "Starting tree-ify")

(define bsp (make-bsp faces))
(define boundary (make-boundary 100.0))

(println "Adding portals")
(add-bsp-portals! bsp boundary)

(println "Marking inside")
(mark-inside! bsp (make-vec3 0 0 10))

(define leafs (bsp-leafs bsp))
(define inside-leafs (filter (lambda (leaf) (plist-ref leaf 'inside?)) leafs))
(define outside-leafs (filter (lambda (leaf) (not (plist-ref leaf 'inside?))) leafs))
(println "Bsp tree complete")

(println "Leaf count: ")
(println (length leafs))

(println "Inside count: ")
(println (length inside-leafs))

(println "Outside count: ")
(println (length outside-leafs))

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

(println "Starting export...")


(println "Solids...")
(define convex-out (list->vector (filter-map (lambda (leaf)
                                               (receive (verts faces)
                                                        (make-indexed-faces leaf)
                                                        (and (positive? (vector-length faces))
                                                             `((vertices . ,verts) (faces . ,faces)))))
                                             (map (lambda (leaf) (plist-ref leaf 'solids))
                                                  inside-leafs))))

(println "Portals...")
(define portals-out (list->vector (filter-map (lambda (portal)
                                                (receive (verts faces)
                                                         (make-indexed-faces portal)
                                                         (and (positive? (vector-length faces))
                                                              `((vertices . ,verts) (faces . ,faces)))))
                                              (map (lambda (portals) (map (lambda (portal) (plist-ref portal 'face)) portals))
                                                   (map (lambda (leaf) (plist-ref leaf 'portals))
                                                        inside-leafs)))))
(call-with-output-file "convex.json" (lambda (port) (json-write convex-out port)))
(call-with-output-file "portal.json" (lambda (port) (json-write portals-out port)))

(println "Export complete")
