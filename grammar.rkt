#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language SP

  ;; program
  (program+
   (import-type ...
    s+ ...))

  ;; imports
  (import-type
   (import-from string (string ...)))

  ;; s+tatements
  (s+ (class x (t+ ...) m+ ...)
      (return e+)
      (claim x t+)
      (define/assign x t+ e+)
      (define/assign (attribute e+ string) t+ e+)
      (define/assign (subscript e+ e+) dynamic e+) ;; sugar
      (def x ([x t+] ...) t+ s+)
      (if e+ s+ s+)
      pass
      (expr e+)
      (delete x)
      (delete (attribute e+ string))
      (delete (subscript e+ e+)) ;; s+ugar
      (begin s+ ...)
      )

  (m+ (field string t+)
      (method string x ((x t+) ...) t+ s+))

  (c number
     boolean
     string
     None)

  ;; expressions
  (e+ x
      c
      (tuple-syntax e+ ...)
      (set-syntax e+ ...)
      (dict-syntax (e+ e+) ...)
      (is e+ e+)
      (is-not e+ e+)
      (if e+ e+ e+)
      (attribute e+ string)
      (e+ e+ ...)
      (reveal-type any ... e+)
      ;; s+ugars
      (oc e+ e+)
      (in e+ e+)
      (subscript e+ e+)
      (bool-op ob e+ ...)
      (unary-op o1 e+)
      (bin-op o2 e+ e+))

  (ob and or)

  (oc < > == <= >=)

  (o2 + -)

  (o ob oc o2)

  (o1 -)

  ;; type expression
  (t+ dynamic
      None ;; nonterminal x doesn't cover t+his because we mentioned None in c
      (subscript x (tuple-syntax t+ ...))
      (subscript x t+)
      (or-syntax t+ t+)
      string  ;; s+ame as x
      x)

  (x variable-not-otherwise-mentioned))

