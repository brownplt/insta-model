#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language SP

  ;; program
  (program
   (import-type ...
    define-class ...
    s ...))

  ;; type imports
  (import-type
   (import-from "__static__" (string ...))
   (import-from "typing" (string ...)))
  
  ;; statements
  (s
   (class x_child (x_parent ...) class-member ...)
   (return e)
   (claim e t)
   (define/assign e t e)
   (define/assign e e)
   (def x ([x t] ...) t s ...)
   pass
   (expr e))

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
     (subscript e e)
     (subscript x (tuple-syntax t ...))
     (attribute e string)
     (e e ...))

  (o2 +)

  ;; type expression
  (t dynamic
     None ;; nonterminal x doesn't cover this because we mentioned None in c
     (subscript x (tuple-syntax t ...))
     x)

  (x variable-not-otherwise-mentioned))

