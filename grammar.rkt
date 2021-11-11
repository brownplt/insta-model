#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language SP

  ;; program
  (program+ (s+ ...))

  ;; constants
  (c number
     boolean
     string
     None)

  ;; boolean operator
  (ob and or)

  ;; cmpop
  (oc < > == <= >= in not-in is is-not)

  ;; bin-op
  (o2 + - * / bit-or)

  ;; unary-op
  (o1 - not)

  ;; expressions
  (e+ x
      (con c)
      (tuple-syntax (e+ ...))
      (set-syntax (e+ ...))
      (dict-syntax ([e+ e+] ...))
      (if-exp e+ e+ e+)
      (attribute e+ x)
      (call e+ (e+ ...))
      (subscript e+ e+)
      (bin-op o2 e+ e+)
      (bool-op ob (e+ ...))
      (unary-op o1 e+)
      (compare e+ ([oc e+] ...))
      (lambda ([x t+] ...) e+))

  ;; type expression
  (t+ dynamic e+)

  ;; statements
  (s+ pass
      (expr e+)
      (return e+)
      (assert e+)
      (if e+ (s+ ...) (s+ ...))
      (delete d-target)
      (ann-assign aa-target t+)
      (ann-assign aa-target t+ e+)
      (assign a-target e+)
      (aug-assign e+ o2 e+)
      (class x (e+ ...) (s+ ...))
      (function-def x ([x t+] ...) t+ (s+ ...))
      (import-from x (x ...))
      (import-from x (*)))

  ;; targets of assignment,
  ;;   which is a subset of e+
  (a-target
   x
   (attribute e+ x)
   (subscript e+ e+)
   (tuple-syntax (a-target ...)))

  ;; targets of ann-assign,
  ;;   which is a subset of assign-target
  (aa-target
   x
   (attribute e+ x))

  ;; targets of deletion,
  ;;   which is a subset of assign-target
  (d-target
   x
   (attribute e+ x)
   (subscript e+ e+))

  (x string))
