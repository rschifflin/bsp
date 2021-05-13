(add-to-load-path (getcwd))
(use-modules (bsp plane)
             (bsp clip))

(define verts '(#f32(0.0 0.0 0.0)
                #f32(1.0 0.0 0.0)
                #f32(1.0 1.0 0.0)
                #f32(0.0 1.0 0.0)))
(define indexed-face '(0 1 2 3))

(define face (map (lambda (idx) (list-ref verts idx)) indexed-face))
(define plane (make-plane #f32(0.5 0.5 0.0) #f32(0.0 1.0 0.0)))

(let* ((clip-result (clip-plane-face plane face))
       (clipped? (eq? 'clipped (car clip-result))))

  (if clipped?
      (let* ((+face (list-ref clip-result 1))
             (-face (list-ref clip-result 2)))
        (display "CLIPPED")
        (newline)
        (display +face)
        (newline)
        (display -face))

      ;; Else, unclipped
      (let ((sign (cadr clip-result)))
        (cond [(eq? sign '=) (display "PLANAR, NOTHING TO CLIP")]
              [(eq? sign '+) (display "POSITIVE, NOTHING TO CLIP")]
              [(eq? sign '-) (display "NEGATIVE, NOTHING TO CLIP")])))
        (newline))
