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
   (class x (t ...) class-member ...)
   (return e)
   (claim x t)
   (define/assign e t e)
   (def x ([x t] ...) t s ...)
   (if e (s ...) (s ...))
   pass
   (expr e)
   (delete e))

  (class-member
   (field string t)
   (method string_method x_self ((x t) ...) t s ...))

  (c number
     boolean
     string
     None)

  (e x
     c
     (tuple-syntax e ...)
     (set-syntax e ...)
     (dict-syntax (e e) ...)
     (is e e)
     (is-not e e)
     (if e e e)
     (attribute e string)
     (e e ...)
     (reveal-type any ... e)
     ;; sugars
     (oc e e)
     (in e e)
     (subscript e e)
     (bool-op ob e ...)
     (unary-op o1 e)
     (bin-op o2 e e))

  (ob and or)

  (oc > ==)

  (o1 -)

  (o2 +)

  ;; type expression
  (t dynamic
     None ;; nonterminal x doesn't cover this because we mentioned None in c
     (subscript x (tuple-syntax t ...))
     (subscript x t)
     (or-syntax t t)
     string  ;; same as x
     x)

  (x variable-not-otherwise-mentioned))

