(define-module (bsp plist)
               #:use-module (srfi srfi-1)
               #:export (plist?
                         plist-ref
                         plist-get
                         plist-put
                         plist-merge
                         ))

;; Predicate for determining if a list is a plist
(define (plist? lst)
  (if (null? lst)
      #t
      (if (null? (cdr lst))
          #f
          (if (symbol? (car lst))
              (plist? (cddr lst))
              #f))))

;; Find element by symbol ref
(define (plist-ref lst sym)
  (if (null? lst)
      #f
      (if (eq? (car lst) sym)
          (cadr lst)
          (plist-ref (cddr lst) sym))))

;; Insert or replace by symbol
(define (plist-put lst sym val)
  (if (null? lst)
      `(,sym ,val)
      (if (eq? (car lst) sym)
          (cons sym (cons val (cddr lst)))
          (cons (car lst) (cons (cadr lst) (plist-put (cddr lst) sym val))))))

;; Modifies target-alist to contain the keys from replacement-alist
(define (plist-merge old-plist new-plist)
  (if (null? new-plist)
      old-plist
      (plist-merge (plist-put old-plist (car new-plist) (cadr new-plist)) (cddr new-plist))))
