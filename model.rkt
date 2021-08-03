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
   (import-from "__static__" (x ...))
   (import-from "typing" (x ...)))

  ;; class definitions
  (define-class
    (class x_child x_parent class-member ...))
  
  ;; statements
  (s
   (return e)
   (define x t e)
   (define x e)
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
     (e e ...))

  ;; base types
  (b None
     bool
     int
     str)

  (t dynamic
     b
     (CheckedDict t t)
     (Callable (t ...) t)
     x)

  (x variable-not-otherwise-mentioned))

