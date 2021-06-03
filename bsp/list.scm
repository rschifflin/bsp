(define-module (bsp list)
               #:export (all?))

(define (all? pred lst)
  (let self ((lst lst))
    (cond [(null? lst) #t]
          [(not (pred (car lst))) #f]
          [else (self (cdr lst))])))
