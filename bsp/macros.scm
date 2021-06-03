(define-module (bsp macros)
               #:export (if*))

(define-syntax if*
  (syntax-rules (?)
    ((if* (ident ? expr) rest ...)
     (let ((ident expr))
      (if ident rest ...)))

    ((if* exprs ...)
     (if exprs ...))))
