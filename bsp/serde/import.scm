(define-module (bsp serde import)
               #:use-module (srfi srfi-43) ; Vector map
               #:use-module (srfi srfi-180) ; Json parsing
               #:use-module (bsp sewer list)
               #:use-module (bsp sewer alist)
               #:use-module (bsp geo face)
               #:use-module (bsp geo vec3)
               #:export (import-scene))

(define (import-scene port)
  (let* ((json-in (json-read port))
         (inside (apply make-vec3 (vector->list (aref json-in 'inside))))
         (meshes (vector->list (aref json-in 'meshes)))
         (faces (flat-map mesh->faces meshes)))
    (list 'inside inside
          'faces faces)))

(define (mesh->faces mesh)
  (vector->list
    (vector-map (lambda (_index face)
                  (vector->list (vector-map (lambda (_index vert-idx)
                                              (let* ((vertex (vector-ref (aref mesh 'vertices) vert-idx)))
                                                (apply make-vec3 (vector->list vertex))))
                                            face)))
                (aref mesh 'faces))))
