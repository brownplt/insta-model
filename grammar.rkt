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
   (claim e t)
   (define/assign e t e)
   (def x ([x t] ...) t s ...)
   (if e (s ...) (s ...))
   pass
   (expr e)
   (delete e))

  (class-member
   (field string t)
   (method string_method x_self ((x t) ...) t s ...))

  (c integer
     boolean
     string
     None
     NotImplemented)

  (e x
     c
     (reveal-type any ... e)
     (set-syntax e ...)
     (dict-syntax (e e) ...)
     (subscript e e)
     (subscript x (tuple-syntax t ...))
     (attribute e string)
     (bool-op ob e ...)
     (unary-op o1 e)
     (bin-op o2 e e)
     (in e e)
     (is e e)
     (is-not e e)
     (if e e e)
     (e e ...))

  (ob and)

  (o1 -)

  (o2 +)

  ;; type expression
  (t dynamic
     None ;; nonterminal x doesn't cover this because we mentioned None in c
     (subscript x (tuple-syntax t ...))
     (subscript x t)
     string  ;; same as x
     x)

  (x variable-not-otherwise-mentioned))

