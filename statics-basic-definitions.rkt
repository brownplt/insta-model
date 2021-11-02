#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "desugar.rkt")
(provide (all-defined-out))

(define-extended-language SP-statics SP-core
  ;; primitive class ids
  (prim-cid "object"
            "float"
            "int"
            "bool"
            "str"
            "dict"
            "set"
            "None"
            ("CheckedDict" cid cid)
            "type")
  ;; class id, an unique data that specify which class it is
  (cid prim-cid
       ;; user defined class
       number)
  ;; cid or dynamic
  (cid+dynamic+☠
   ;; parent class
   cid
   ;; subclass from a dynamic class
   dynamic
   ;; the root / object class
   ☠)
  (C+☠ C ☠)
  ;; class description
  (C (class any
       ;; parents
       cid+dynamic+☠
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
  (Ψ ((cid C+☠) ...))
  ;; a local environment that maps variables to their types
  (Γ ((x T) ...))
  ;; extension to scope
  (ext ([x kind] ...))
  (kind def/cls var)
  ;; syntactic types
  (t .... (quote T))
  ;;
  (T+☠ T ☠)
  ;; semantic types / type values
  (T dynamic
     ;; class instances
     (instancesof cid)
     ;; classes themselves, useful in instance construction
     (classitself cid)
     ;; procedures
     (-> ([x+☠ T] ...) T)
     ;; special types, e.g. optional
     ("optional" cid)
     ;; These are not really types but type-level things
     (prim-generic string)
     (type-op "cast")
     )
  ;; optional variable
  (x+☠ ☠ x)
  ;; variable context
  (vctx global local)
  )
