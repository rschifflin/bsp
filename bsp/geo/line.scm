(define-module (bsp geo line)
               #:use-module (srfi srfi-1)
               #:use-module (bsp geo vec3)
               #:export (make-line
                         make-line-from-points
                         line-point
                         line-dir
                         line-segments-overlap?
                         ))

(define (make-line point dir)
  (cons point dir))

(define (make-line-from-points p0 p1)
  ;; Tricky edge case:
  ;; A line from a->b might be subtly different than b->a due to floating point error.
  ;; Therefore we use a strict ordering of points so that we always return the same line
  ;; for the same two points
  (make-line p0 (v3:norm (apply v3:sub (v3:ord > p0 p1)))))

(define (line-point line)
  (car line))

(define (line-dir line)
  (cdr line))

;; Helper function to determine if two co-linear segments defined by the lists (a0 a1) and (b0 b1), where ax and bx are vec3 points,
;; overlap or not.
(define (line-segments-overlap? segment1 segment2)
  (let* ((a0 (first segment1))
         (a1 (second segment1))
         (bi (first segment2))  ;; The ordering of b0 and b1 are unclear
         (bj (second segment2)) ;; Label i and j initially until ordering is known

         ;; If the segment2 vector points opposite the segment1 vector, flip it
         ;; This orders segment2 such that the vector b1-b0 points the same direction as the vector a1-a0
         (segment2 (if (< (v3:dot (v3:sub a1 a0)
                                  (v3:sub bj bi))
                               0)
                            (reverse segment2)
                            segment2))
         (b0 (first segment2))
         (b1 (second segment2)))

    ;; If any of the following conditions hold, the line segments must overlap:
    (or (v3:~= b0 a0)                                   ;; b0 ~= a0
        (v3:~= b1 a1)                                   ;; b1 ~= a1
        (> (v3:dot (v3:sub b0 a0) (v3:sub a1 b0)) 0)    ;; b0 between a0 and a1
        (> (v3:dot (v3:sub b1 a0) (v3:sub a1 b1)) 0)    ;; b1 between a0 and a1
        (> (v3:dot (v3:sub a0 b0) (v3:sub b1 a0)) 0)    ;; a0 between b0 and b1
        (> (v3:dot (v3:sub a1 b0) (v3:sub b1 a1)) 0)))) ;; a1 between b0 and b1

