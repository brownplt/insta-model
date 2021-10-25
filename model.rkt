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

  ;; imports
  (import-type
   (import-from string (string ...)))
  
  ;; statements
  (s
   (class x_child (x_parent ...) class-member ...)
   (return e)
   (claim e t)
   (define/assign e t e)
   (def x ([x t] ...) t s ...)
   pass
   (expr e)
   (delete e))

  (class-member
   (field string t)
   (method string_method x_self ((x_arg t) ...) t s ...))

  (c integer
     boolean
     string
     None
     NotImplemented)

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

