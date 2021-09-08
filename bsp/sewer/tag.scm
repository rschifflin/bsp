;;; Defines an object inheritance convention called tags
;;; Tags are simply a type-tag wrapper around a value
(define-module (bsp sewer tag)
               #:use-module (oop goops)
               #:export (define-tag tag untag tag-set!))

(define-class <type-tag-meta> (<class>))
(define-class <type-tag> ()
              (value #:init-keyword #:value)
              #:metaclass <type-tag-meta>)
(define-method (tag value (type-tag <type-tag-meta>))
                      (make type-tag #:value value))

(define-method (untag (type-tag <type-tag>))
                      (slot-ref type-tag 'value))

(define-method (tag-set! (type-tag <type-tag>) val)
                      (slot-set! type-tag 'value val))

(define-syntax define-tag
  (syntax-rules (:)
    ((define-tag name : trait ...)
     (begin
       (define-class name (<type-tag> trait ...))))
    ((define-tag name)
     (define-tag name :))))
