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
   pass
   e)

  (class-member
   (field x t)
   (method x_method x_self ((x_arg t_arg) ...) t_ret s ...))

  (i number)
  (b boolean)
  (c i
     b)

  (e x
     c)

  (t dynamic
     None
     bool
     int
     str
     (CheckedDict t t)
     (class x))

  (x variable-not-otherwise-mentioned))
