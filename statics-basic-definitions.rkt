#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))

(define-extended-language SP-statics SP
  ;; a global environment that maps class names to their definitions
  (Ψ ((any T) ...))
  ;; a local environment that maps variables to their types
  (Γ ((x T) ...))
  ;; syntactic types
  (t .... (quote T))
  ;; semantic types / type values
  (T dynamic
     (the-object-class) ;; the object class
     (prim-generic string)
     (generic string T ...)
     (-> ([x+☠ t] ...) t)
     (class any (x_parent ...)
       ((string_field t) ...)
       ((string_method ([x+☠ t] ...) t) ...))
     ;; classes themselves, useful in instance construction
     (class T))
  ;; a handy concept that is useful when I want to check well-formness of classes
  (flat-class (((string_field t_field) ...)
               ((string_method ([x+☠ t] ...) t) ...)))
  ;; optional flat-class
  (flat-class+☠ flat-class ☠)
  ;; optional variable
  (x+☠ ☠ x)
  ;; primtive class names
  (prim-class-name
   "dynamic"
   "object"
   "float"
   "int"
   "bool"
   "str"
   "dict"
   "None-class")
  )
