;;; Defines an object inheritance convention called traits
;;; Traits have a single inheritance chain from other traits
;;; Traits have no fields and define a virtual method interface
;;; Objects that implement the trait sit at the leaf of the
;;; inheritance tree and may multiple-inherit as many traits
;;; as they wish.
(define-module (bsp sewer trait)
               #:use-module (oop goops)
               #:export (<trait> define-trait)
               #:re-export (define-method))

(define-class <trait> (<class>))
(define-syntax define-trait
  (syntax-rules (<self> :)
    ;; Full syntax
    ((define-trait name : supertrait ((method-name (self <self>) (arg type) ...) ...))
     (begin
       (define-class name (supertrait) #:metaclass <trait>)
       (define-method (method-name (self name) (arg type) ...)
                      (error "Trait not implemented for object." name self)) ...))

    ;; Without explicit supertrait defaults to none
    ((define-trait name ((method-name (self <self>) (arg type) ...) ...))
     (begin
       (define-class name () #:metaclass <trait>)
       (define-method (method-name (self name) (arg type) ...)
                      (error "Trait not implemented for object." name self)) ...))))
