#lang racket
(require redex)
(require redex-abbrevs)
(provide (all-defined-out))

(define-language SP

  ;; program
  (program
   (import-type ...
    s ...))

  ;; imports
  (import-type
   (import-from string (string ...)))
  
  ;; statements
  (s (class x (t ...) m ...)
     (return e)
     (claim x t)
     (define/assign x t e)
     (define/assign (attribute e string) dynamic e)
     (define/assign (subscript e e) dynamic e) ;; sugar
     (def x ([x t] ...) t s)
     (if e s s)
     (begin s ...)
     pass
     (expr e)
     (delete x)
     (delete (attribute e string))
     (delete (subscript e e)) ;; sugar
     )

  (m (field string t)
     (method string x ((x t) ...) t s))

  (c number
     boolean
     string
     None)

  ;; expressions
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

  (oc < > == <= >=)

  (o2 + -)

  (o ob oc o2)
  
  (o1 -)

  ;; type expression
  (t dynamic
     None ;; nonterminal x doesn't cover this because we mentioned None in c
     (subscript x (tuple-syntax t ...))
     (subscript x t)
     (or-syntax t t)
     string  ;; same as x
     x)

  (x variable-not-otherwise-mentioned))


;; The remaining part of this file describe the desugaring process.
;; Desugaring is context-insensitive -- it doesn't rely on the type of
;; terms. This process remove two kinds of constructs:
;;   - operators (except is and is-not because they are primitive)
;;   - restrict assignment targets to variables and attributes

(module+ test
  (test-equal (term (method-name <)) (term "__lt__"))
  (test-equal (term (method-name >)) (term "__gt__"))
  (test-equal (term (method-name ==)) (term "__eq__"))
  (test-equal (term (method-name >=)) (term "__ge__"))
  (test-equal (term (method-name <=)) (term "__le__"))
  (test-equal (term (method-name and)) (term "__and__"))
  (test-equal (term (method-name or)) (term "__or__"))
  (test-equal (term (method-name +)) (term "__add__")))

(define-metafunction SP
  method-name : o -> string
  [(method-name <) "__lt__"]
  [(method-name >) "__gt__"]
  [(method-name ==) "__eq__"]
  [(method-name >=) "__ge__"]
  [(method-name <=) "__le__"]
  [(method-name and) "__and__"]
  [(method-name or) "__or__"]
  [(method-name +) "__add__"]
  [(method-name -) "__sub__"])

(module+ test
  (test-equal (term (desugar-e xyz))
              (term xyz))
  (test-equal (term (desugar-e 42))
              (term 42))
  (test-equal (term (desugar-e (> (== 2 3) (>= 4 5))))
              (term ((attribute ((attribute 2 "__eq__") 3) "__gt__") ((attribute 4 "__ge__") 5))))
  (test-equal (term (desugar-e (in (== 2 3) (>= 4 5))))
              (term ((attribute ((attribute 4 "__ge__") 5) "__contains__") ((attribute 2 "__eq__") 3))))
  (test-equal (term (desugar-e (subscript (tuple-syntax 2 3) 0)))
              (term ((attribute (tuple-syntax 2 3) "__getitem__") 0)))
  (test-equal (term (desugar-e (bool-op and 0)))
              (term 0))
  (test-equal (term (desugar-e (bool-op or 0 1)))
              (term ((attribute 0 "__or__") 1)))
  (test-equal (term (desugar-e (bin-op + 2 3)))
              (term ((attribute 2 "__add__") 3)))
  (test-equal (term (desugar-e (unary-op - 2)))
              (term ((attribute 2 "__neg__")))))

(define-metafunction SP
  desugar-e : e -> e
  [(desugar-e x) x]
  [(desugar-e c) c]
  [(desugar-e (tuple-syntax e ...))
   (tuple-syntax (desugar-e e) ...)]
  [(desugar-e (set-syntax e ...))
   (set-syntax (desugar-e e) ...)]
  [(desugar-e (dict-syntax [e_key e_val] ...))
   (dict-syntax [(desugar-e e_key) (desugar-e e_val)] ...)]
  [(desugar-e (is e_1 e_2))
   (is (desugar-e e_1) (desugar-e e_2))]
  [(desugar-e (is-not e_1 e_2))
   (is-not (desugar-e e_1) (desugar-e e_2))]
  [(desugar-e (if e_cnd e_thn e_els))
   (if (desugar-e e_cnd) (desugar-e e_thn) (desugar-e e_els))]
  [(desugar-e (attribute e string))
   (attribute (desugar-e e) string)]
  [(desugar-e (e_fun e_arg ...))
   ((desugar-e e_fun) (desugar-e e_arg) ...)]
  [(desugar-e (reveal-type any ... e))
   (reveal-type any ... (desugar-e e))]
  ;; All below are interesting cases
  [(desugar-e (oc e_1 e_2))
   ((attribute (desugar-e e_1) (method-name oc)) (desugar-e e_2))]
  [(desugar-e (in e_1 e_2))
   ((attribute (desugar-e e_2) "__contains__") (desugar-e e_1))]
  [(desugar-e (subscript e_1 e_2))
   ((attribute (desugar-e e_1) "__getitem__") (desugar-e e_2))]
  [(desugar-e (bool-op ob e))
   (desugar-e e)]
  [(desugar-e (bool-op ob e_1 e_2 ...))
   ((attribute (desugar-e e_1) (method-name ob)) (desugar-e (bool-op ob e_2 ...)))]
  [(desugar-e (bin-op o2 e_1 e_2))
   ((attribute (desugar-e e_1) (method-name o2)) (desugar-e e_2))]
  [(desugar-e (unary-op - e))
   ((attribute (desugar-e e) "__neg__"))])


(define-metafunction SP
  desugar-s : s -> s
  [(desugar-s (class x (t ...) m ...))
   (class x (t ...) (desugar-m m) ...)]
  [(desugar-s (def x ([x_arg t_arg] ...) t_ret s))
   (def x ([x_arg (desugar-e t_arg)] ...) (desugar-e t_ret) (desugar s))]
  [(desugar-s (claim x e))
   (claim x (desugar-e e))]
  [(desugar-s (define/assign x t e))
   (define/assign x (desugar-e t) (desugar-e e))]
  [(desugar-s (define/assign (attribute e_map string_key) dynamic e_val))
   (define/assign (attribute (desugar-e e_map) string_key) dynamic (desugar-e e_val))]
  ;; Interesting case
  [(desugar-s (define/assign (subscript e_map e_key) dynamic e_val))
   (expr ((attribute e_map "__setitem__") dynamic (desugar-e e_val)))]
  [(desugar-s (return e))
   (return (desugar-e e))]
  [(desugar-s (if e s s))
   (if (desugar-e e) (desugar-s s) (desugar-s s))]
  [(desugar-s (begin s ...))
   (begin (desugar-s s) ...)]
  [(desugar-s pass)
   pass]
  [(desugar-s (delete x))
   (delete x)]
  [(desugar-s (delete (attribute e string)))
   (delete (attribute (desugar-e e) string))]
  ;; Interesting case
  [(desugar-s (delete (subscript e_map e_key)))
   (expr ((attribute (desugar-e e_map) "__delitem__") (desugar-e e_key)))]
  [(desugar-s (expr e))
   (expr (desugar-e e))]
  )

(define-metafunction SP
  desugar-m : m -> m
  [(desugar-m (field string t))
   (field string t)]
  [(desugar-m (method string x_slf ([x_arg t_arg] ...) t_ret s))
   (method string x_slf ([x_arg (desugar-t t_arg)] ...) (desugar-t t_ret) (desugar-s s))])

(define-metafunction SP
  desugar-t : t -> t
  [(desugar-t dynamic) dynamic]
  [(desugar-t None) None]
  [(desugar-t (subscript x (tuple-syntax t ...)))
   ((attribute x "__getitem__") (tuple-syntax (desugar-t t) ...))]
  [(desugar-t (subscript x t))
   ((attribute x "__getitem__") (desugar-t t))]
  [(desugar-t (or-syntax t_lft t_rht))
   (or-syntax (desugar-t t_lft) (desugar-t t_rht))]
  [(desugar-t string) ,(string->symbol (term string))]
  [(desugar-t x) x])

(define-metafunction SP
  desugar-program : program -> program
  [(desugar-program (import-type ... s ...))
   (desugar-program (import-type ... (desugar-s s) ...))])

