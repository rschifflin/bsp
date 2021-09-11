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

(define-syntax define-trait-methods
  (syntax-rules (<self>)
    ((define-trait-methods name ((method-name (self <self>) rest ...) ...))
     (begin
       (define-method (method-name (self name) rest ...)
                      (error "Method on trait not implemented for object." method-name name self)) ...))))

(define-syntax define-trait
  (syntax-rules (:)
    ;; Full syntax
    ((define-trait name : supertrait methods)
     (begin
       (define-class name (supertrait) #:metaclass <trait>)
       (define-trait-methods name methods)))

    ;; Without explicit supertrait defaults to none
    ((define-trait name methods)
     (begin
       (define-class name () #:metaclass <trait>)
       (define-trait-methods name methods)))))
