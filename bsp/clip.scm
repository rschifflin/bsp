(define-module (bsp clip)
               #:use-module (srfi srfi-1)
               #:use-module (bsp alist)
               #:use-module (bsp plane)
               #:use-module (bsp line)
               #:export (clip-plane-face))

;;; Clip a face by a plane
;;; Returns ADT, one of:
;;;   ('clipped  tail-verts +face -face)
;;;   ('unclipped <+|-|=>)
(define (clip-plane-face plane face verts)
  (let* ((point-fn (lambda (i) (list-ref verts i)))
         (point-count (length verts))
         (points (map point-fn face))
         (test-result (plane-test-face plane points))
         (needs-clip? (car test-result)))

    ;; clip props are an alist
    ;; '(+face . (indices...))
    ;; '(-face . (indices...))
    ;; '(prev . (face-idx charge))
    ;; '(tail-verts . (points...))
    (define (clip-point idx,sign props)
      (let ((+face (aref props '+face))
            (-face (aref props '-face))
            (prev-idx (car (aref props 'prev)))
            (prev-sign (cadr (aref props 'prev)))
            (tail-verts (aref props 'tail-verts))
            (idx (car idx,sign))
            (sign (cadr idx,sign)))

        (cond [(eq? sign '=) ; Planar case; add the idx to both faces
               (let ((+face (cons idx +face))
                     (-face (cons idx -face)))
                 (amerge props `((+face . ,+face)
                                 (-face . ,-face)
                                 (prev . ,idx,sign))))]

              [(and (eq? sign '+)
                    (not (eq? prev-sign '-))) ; Positive non-intersecting case; add idx to positive face
               (let ((+face (cons idx +face)))
                    (amerge props `((+face . ,+face)
                                    (prev . ,idx,sign))))]

              [(and (eq? sign '-)
                    (not (eq? prev-sign '+))) ; Negative non-intersecting case; add idx to negative face
               (let ((-face (cons idx -face)))
                    (amerge props `((-face . ,-face)
                                    (prev . ,idx,sign))))]

              [else ; intersecting case; generate intermediate planar vertex. Add to both faces and update tail-verts
                (let* ((point-fn (lambda (i) (list-ref verts i)))
                       (line (make-line-from-points (point-fn prev-idx)
                                                    (point-fn idx)))
                       (vert (plane-line-intersection plane line))
                       (vert-idx (+ point-count (length tail-verts)))
                       (+face (if (eq? sign '+)
                                  (cons idx (cons vert-idx +face))
                                  (cons vert-idx +face)))
                       (-face (if (eq? sign '-)
                                  (cons idx (cons vert-idx -face))
                                  (cons vert-idx -face)))
                       (tail-verts (cons vert tail-verts)))
                  (amerge props `((+face . ,+face)
                                  (-face . ,-face)
                                  (prev . ,idx,sign)
                                  (tail-verts . ,tail-verts))))])))

    (define (clip signs)
      (let* ((face+signs (zip face signs))
             (p0    (car face+signs))
             (ptail (cdr face+signs))
             (props `((+face . ())
                      (-face . ())
                      (prev . ,p0)
                      (tail-verts . ())))
             (props (clip-point p0 (fold clip-point props ptail)))
             (tail-verts (reverse (aref props 'tail-verts)))
             (+face (reverse (aref props '+face)))
             (-face (reverse (aref props '-face))))

          `(,tail-verts ,+face ,-face)))

    (if needs-clip?
      `(clipped . ,(clip (cadr test-result)))
      `(unclipped ,(cadr test-result)))))
