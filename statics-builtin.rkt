#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "statics-basic-definitions.rkt")
(require "statics-utilities.rkt")
(provide (all-defined-out))

(define-metafunction SP-statics
  prim-class : prim-class-name -> T
  [(prim-class prim-class-name)
   (lookup (base-Ψ) ,(string->symbol (term prim-class-name)))])

(define-metafunction SP-statics
  base-Ψ : -> Ψ
  [(base-Ψ) ((object (the-object-class))
             (type
              (class "type" (object)
                ()
                ()))
             (float
              (class "float" (object)
                ()
                (("__init__" ([☠ dynamic]) None)
                 ("__add__" ([☠ float] [☠ float]) float))))
             (int (class "int" (float) () ()))
             (bool (class "bool" (int) () ()))
             (str
              (class "str" (object)
                ()
                (("__init__" ([☠ dynamic]) None))))
             (dict
              (class "dict" (object)
                ()
                (("__init__" ([☠ dynamic]) None)
                 ("__getitem__" ([☠ dynamic]) dynamic))))
             (Callable (prim-generic "Callable"))
             (CheckedDict (prim-generic "CheckedDict"))
             (None-class
              (class "None" (object)
                ()
                (("__init__" ([☠ dynamic]) None)))))])

(define-metafunction SP-statics
  base-Γ : -> Γ
  [(base-Γ)
   ([x (class T)] ...)
   (where ([x T] ...) (base-Ψ))])