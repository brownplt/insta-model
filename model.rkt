#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language StaticPython

  ;; program
  (program
   (define-class ...
    s ...))

  ;;
  (define-class
    (class x_child x_parent class-member ...))
  
  ;; statements
  (s
   (return e)
   (define x t e)
   pass
   e)

  (class-member
   (field x t)
   (method x_method x_self ((x_arg t_arg) ...) t_ret s ...))

  (c integer
     boolean
     string)

  (e x
     c)

  (t dynamic
     None
     bool
     int
     str
     (CheckedDict t t)
     (Callable (t ...) t)
     x)

  (x variable-not-otherwise-mentioned))



(module+ test
  (check-judgment-holds*
   (≲ int int)
   (≲ int dynamic)
   (≲ dynamic int)
   (≲ bool int)
   (≲ (Callable (int) bool) (Callable (int) int))
   (≲ (Callable (int) int) (Callable (bool) int))))

(define-judgment-form StaticPython
  #:mode (≲ I I)
  #:contract (≲ t t)
  ;; Is it sensible to use a value of type t_0 as as value of type t_1?
  ;; (consistent subtyping)
  
  [------------------------ "dynamic-R"
   (≲ t dynamic)]

  [------------------------ "dynamic-L"
   (≲ dynamic t)]

  [------------------------ "bool≲int"
   (≲ bool int)]

  [------------------------ "tag-bool"
   (≲ bool bool)]

  [------------------------ "tag-int"
   (≲ int int)]

  [------------------------ "tag-str"
   (≲ str str)]

  [(≲ t_1i t_0i) ...
   (≲ t_0o t_1o)
   ------------------------ "tag-callable"
   (≲ (Callable (t_0i ...) t_0o)
      (Callable (t_1i ...) t_1o))]

  )