(define-module (bsp clip)
               #:use-module (srfi srfi-1)
               #:use-module (bsp alist)
               #:use-module (bsp plane)
               #:use-module (bsp line)
               #:export (clip-plane-face))

;;; Clip a face by a plane
;;; Returns ADT, one of:
;;;   ('clipped +face -face)
;;;   ('unclipped <+|-|=>)
(define (clip-plane-face plane face)
  (let* ((test-result (plane-test-face plane face))
         (needs-clip? (car test-result)))

    ;; clip props are an alist
    ;; '(+face . (indices...))
    ;; '(-face . (indices...))
    ;; '(prev . (point sign))
    (define (clip-point point,sign props)
      (let ((+face (aref props '+face))
            (-face (aref props '-face))
            (prev-point (car (aref props 'prev)))
            (prev-sign (cadr (aref props 'prev)))
            (point (car point,sign))
            (sign (cadr point,sign)))

        (cond [(eq? sign '=) ; Planar case; add the idx to both faces
               (let ((+face (cons point +face))
                     (-face (cons point -face)))
                 (amerge props `((+face . ,+face)
                                 (-face . ,-face)
                                 (prev . ,point,sign))))]

              [(and (eq? sign '+)
                    (not (eq? prev-sign '-))) ; Positive non-intersecting case; add idx to positive face
               (let ((+face (cons point +face)))
                    (amerge props `((+face . ,+face)
                                    (prev . ,point,sign))))]

              [(and (eq? sign '-)
                    (not (eq? prev-sign '+))) ; Negative non-intersecting case; add idx to negative face
               (let ((-face (cons point -face)))
                    (amerge props `((-face . ,-face)
                                    (prev . ,point,sign))))]

              [else ; intersecting case; generate intermediate planar vertex. Add to both faces
                (let* ((line (make-line-from-points prev-point point))
                       (vert (plane-line-intersection plane line))
                       (+face (if (eq? sign '+)
                                  (cons point (cons vert +face))
                                  (cons vert +face)))
                       (-face (if (eq? sign '-)
                                  (cons point (cons vert -face))
                                  (cons vert -face))))
                  (amerge props `((+face . ,+face)
                                  (-face . ,-face)
                                  (prev . ,point,sign))))])))

    (define (clip signs)
      (let* ((face+signs (zip face signs))
             (p0    (car face+signs))
             (ptail (cdr face+signs))
             (props `((+face . ())
                      (-face . ())
                      (prev . ,p0)))
             (props (clip-point p0 (fold clip-point props ptail)))
             (+face (reverse (aref props '+face)))
             (-face (reverse (aref props '-face))))
          `(,+face ,-face)))

    (if needs-clip?
      `(clipped . ,(clip (cadr test-result)))
      `(unclipped ,(cadr test-result)))))
