#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language StaticPython

  ;; program
  (program
   (define-class ...
    s ...))

  ;;
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
     (e e ...))

  (t dynamic
     None
     bool
     int
     str
     (CheckedDict t t)
     (Callable (t ...) t)
     x)

  (x variable-not-otherwise-mentioned))

