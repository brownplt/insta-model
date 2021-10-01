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
     (dict (e e) ...)
     (subscript e e)
     (tuple t t)
     (e e ...))

  (t dynamic
     variable
     (subscript variable (tuple t ...))
     (Callable (t ...) t)
     x)

  (x variable-not-otherwise-mentioned))

