#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language StaticPython

  ;; program
  (program
   (top-statement ...))
  
  ;; general purpose statements
  (s
   top-statement
   method-statement)
  
  (top-statement
   (define x t e)
   pass
   (class x_child x_parent class-member ...)
   e)

  (method-statement
   (return e)
   (define x t e)
   pass
   e)

  (class-member
   (field x t)
   (method x_method x_self ((x_arg t_arg) ...) t_ret method-statement ...))

  (c number
     boolean)

  (e c)

  (t dynamic
     None
     bool
     int
     str
     (CheckedDict t t))

  (C (class x_child x_parent
       ((x_field t_field) ...)
       ((x_method x_self (t_arg ...) t_ret) ...)))

  (x variable-not-otherwise-mentioned))
