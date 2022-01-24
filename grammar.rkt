#lang racket
(require redex/reduction-semantics)
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
      (lambda ([x t+] ...) e+))

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
      (delete delete-target)
      (ann-assign ann-assign-target t+)
      (ann-assign ann-assign-target t+ e+)
      (assign (a-target ...) e+)
      (aug-assign e+ o2 e+)
      (class x (e+ ...) (s+ ...))
      (function-def x ([x t+] ...) t+ (s+ ...))
      (import-from x (x ...))
      (import-from x (*))
      (raise e+)
      (try (s+ ...) (h+ ...) (s+ ...) (s+ ...)))

  ;; except-handlers
  (h+ (except-handler e++None x+None (s+ ...)))
  (e++None e+ None)
  (x+None x None)


  ;; targets of ann-assign
  (ann-assign-target
   x
   (attribute e+ x))

  ;; targets of delete
  (delete-target
   ann-assign-target
   (subscript e+ e+))
  
  ;; targets of assign
  (a-target
   delete-target
   (tuple (a-target ...))
   (list (a-target ...)))

  (x string))
