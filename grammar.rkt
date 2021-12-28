#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language SP

  ;; program
  (program+ (s+ ...))

  ;; constants
  (c None
     boolean
     integer
     string)

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
      (tuple (e+ ...))
      (list (e+ ...))
      (set (e+ ...))
      (dict ([e+ e+] ...))
      (unary-op o1 e+)
      (bin-op o2 e+ e+)
      (bool-op ob (e+ ...))
      (compare e+ ([oc e+] ...))
      (if-exp e+ e+ e+)
      (attribute e+ x)
      (call e+ (e+ ...))
      (subscript e+ e+)
      (lambda ([x t+] ...) e+)
      (list-comp e+ (g+ ...)))

  ;; generator (comprehension node)
  (g+ [a-target e+ (e+ ...)])

  ;; type expression
  (t+ dynamic e+)

  ;; statements
  (s+ pass
      (expr e+)
      (return e+)
      (assert e+)
      (if e+ (s+ ...) (s+ ...))
      (while e+ (s+ ...) (s+ ...))
      (for a-target e+ (s+ ...) (s+ ...))
      break
      continue
      (delete d-target)
      (ann-assign aa-target t+)
      (ann-assign aa-target t+ e+)
      (assign (a-target ...) e+)
      (aug-assign e+ o2 e+)
      (class x (e+ ...) (s+ ...))
      (function-def x ([x t+] ...) t+ (s+ ...))
      (import-from x (x ...))
      (import-from x (*))
      (try-except-else-finally (s+ ...) (h+ ...) (s+ ...) (s+ ...))
      (raise e+))

  ;; except-handlers
  (h+ (except-handler e+ x+None (s+ ...)))
  (x+None x None)

  ;; targets of assignment,
  ;;   which is a subset of e+
  (a-target
   x
   (attribute e+ x)
   (subscript e+ e+)
   (tuple (a-target ...))
   (list (a-target ...)))

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
