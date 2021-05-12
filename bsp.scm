(add-to-load-path (getcwd))
(use-modules (bsp list)
             (bsp alist)
             (bsp vec3)
             (bsp plane)
             (bsp clip))

#|
(display all?)
(display aref)
(display aset!)
(display amerge)
(display make-vec3)
(display v3:sub)
(display v3:cross)
(display v3:dot)
(display v3:norm)
(display make-plane)
(display plane-point)
(display plane-normal)
(display plane-cmp-point)
(display plane-line-intersection)
(display clip-plane-face)
|#

(define verts '((0.0 0.0 0.0)
                (1.0 0.0 0.0)
                (1.0 1.0 0.0)
                (0.0 1.0 0.0)))
(define face '(0 1 2 3))
(define plane (make-plane '(0.5 0.0 0.0) '(0.0 1.0 0.0)))

(let* ((clip-result (clip-plane-face plane face verts))
       (clipped? (eq? 'clipped (car clip-result))))

  (if clipped?
      (let* ((verts (append verts (cadr clip-result)))
             (+face (list-ref clip-result 2))
             (-face (list-ref clip-result 3))
             (idx->vert (lambda (face-idx) (list-ref verts face-idx))))
        (display "CLIPPED")
        (newline)
        (display (map idx->vert +face))
        (newline)
        (display (map idx->vert -face)))

      ;; Else, unclipped
      (let ((sign (cadr clip-result)))
        (cond [(eq? sign '=) (display "PLANAR, NOTHING TO CLIP")]
              [(eq? sign '+) (display "POSITIVE, NOTHING TO CLIP")]
              [(eq? sign '-) (display "NEGATIVE, NOTHING TO CLIP")])))
        (newline))
