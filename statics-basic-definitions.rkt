#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))

(define-extended-language SP-statics SP
  ;; primitive class ids
  (prim-cid "object"
            "float"
            "int"
            "bool"
            "str"
            "dict"
            "None"
            ("CheckedDict" cid cid)
            "type")
  ;; class id, an unique data that specify which class it is
  (cid prim-cid
       ;; user defined class
       number)
  ;; class description
  (C (class any
       ;; parents
       (cid ...)
       ;; fields
       ((string T) ...)
       ;; methods
       ((string ([x+☠ T] ...) T) ...)))
  ;; a handy concept that is useful when I want to check well-formness of classes
  (flat-class (((string T) ...)
               ((string ([x+☠ T] ...) T) ...)))
  ;; optional flat-class
  (flat-class+☠ flat-class ☠)
  ;; a global environment that maps class ids to their definitions
  (Ψ ((cid C) ...))
  ;; a local environment that maps variables to their types
  (Γ ((x T) ...))
  ;; syntactic types
  (t .... (quote T))
  ;; semantic types / type values
  (T dynamic
     (-> ([x+☠ T] ...) T)
     ;; class instances
     (instancesof cid)
     ;; classes themselves, useful in instance construction
     (classitself cid)
     ;; special types
     (prim-generic string)
     (type-op "cast")
     )
  ;; optional variable
  (x+☠ ☠ x)
  )
