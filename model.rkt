#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language StaticPython

  ;; program
  (program
   (import-type ...
    define-class ...
    s ...))

  ;; type imports
  (import-type
   (import-from "__static__" (any ...))
   (import-from "typing" (any ...)))
  
  ;; statements
  (s
   (class x_child x_parent class-member ...)
   (return e)
   (define/assign e t e)
   (define/assign e e)
   (delete e)
   pass
   e)

  (class-member
   (field string t)
   (method string_method x_self ((x_arg t_arg) ...) t_ret s ...))

  (c integer
     boolean
     string
     None)

  (e x
     c
     (dict-syntax (e e) ...)
     (tuple-syntax t t)
     (subscript e e)
     (e o2 e)
     (e e ...))

  (o2 +)

  ;; type expression
  (t dynamic
     None ;; x doesn't cover this because we mentioned None in c
     (subscript x (tuple-syntax t ...))
     x)

  (x variable-not-otherwise-mentioned))
