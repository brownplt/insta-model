#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language SP

  ;; program
  (program+
   (import-from-item+ ...
    s+ ...))

  ;; imports
  (import-from-item+ 
    (import-from string (x ...))
    (import-from string (*)))
  
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
      c
      (tuple-syntax e+ ...)
      (set-syntax e+ ...)
      (dict-syntax [e+ e+] ...)
      (if-exp e+ e+ e+)
      (attribute e+ string)
      (call e+ e+ ...)
      (reveal-type any ... e+)
      (subscript e+ e+)
      (bin-op o2 e+ e+)
      (bool-op ob e+ ...)
      (unary-op o1 e+)
      (lambda ([x t+] ...) e+)
      (compare e+ ([oc e+] ...)))

  ;; type expression
  (t+ dynamic e+)
  
  ;; targets of assignment
  (target x
          (attribute e+ string)
          (subscript e+ e+)
          (tuple-syntax target ...))
  
  ;; statements
  (s+ pass
      (expr e+)
      (return e+)
      (assert e+)
      (if e+ [s+ ...] [s+ ...])
      (delete target)
      (ann-assign target t+)
      (ann-assign target t+ e+)
      (assign target e+)
      (aug-assign e+ o2 e+)
      (class x (e+ ...) s+ ...)
      (function-def x ([x t+] ...) t+ s+ ...))

  (x variable-not-otherwise-mentioned))
