#lang racket
(require redex)
(require "grammar.rkt")
(provide (all-defined-out))

(define-extended-language SP-core SP

  ;; program
  (program (import-type ... d s))

  ;; declaration
  (d ([x t] ...))

  ;; statements
  (s (define/assign x t e)
     (define/assign (attribute e string) t e)
     (def x ([x t] ...) t d s)
     (class x (t ...) m ...)
     (if e s s)
     (begin s ...)
     (delete x)
     (delete (attribute e string))
     (return e)
     (expr e)
     pass
     )

  (m (field string t)
     (method string x ([x t] ...) t d s))

  ;; type expression
  (t dynamic
     None ;; nonterminal x doesn't cover this because we mentioned None in c
     ((attribute x string) (tuple-syntax t ...))
     ((attribute x string) t)
     (or-syntax t t)
     x
     ;; new stufff
     (def ([x t] ...) t)
     (class x (t ...) m ...)
     )

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
     (bool-op ob e e)))


;; The remaining part of this file describe the desugaring process.
;; Desugaring is context-insensitive -- it doesn't rely on the type of
;; terms. This process remove three kinds of constructs:
;;   - operators (except is and is-not because they are primitive)
;;   - subscription
;;   - restrict assignment targets to variables and attributes
;; And lift variable declaration to the beginning of each block

(define-metafunction SP-core
  append : (any ...) (any ...) -> (any ...)
  [(append (any_1 ...) (any_2 ...))
   (any_1 ... any_2 ...)])

(module+ test
  (test-equal (term (lift-claims (define/assign xyz int 42)))
              (term ([xyz int])))
  (test-equal (term (lift-claims (define/assign (attribute objx "unknown") int 42)))
              (term ()))
  (test-equal (term (lift-claims (def func ([n int]) str () (return None))))
              (term ([func (def ([n int]) str)])))
  (test-equal (term (lift-claims (class MyClass (int str))))
              (term ([MyClass (class MyClass (int str))])))
  (test-equal (term (lift-claims (if (bool-op and 0 1)
                                     (define/assign x int 42)
                                     (define/assign y str "foo"))))
              (term ([x int] [y str])))
  (test-equal (term (lift-claims (begin (define/assign x int 42)(define/assign y str "foo"))))
              (term ([x int] [y str])))
  (test-equal (term (lift-claims (delete xyz)))
              (term ()))
  (test-equal (term (lift-claims (delete (attribute x "__class__"))))
              (term ()))
  (test-equal (term (lift-claims (return 42)))
              (term ()))
  (test-equal (term (lift-claims (expr "foo")))
              (term ()))
  (test-equal (term (lift-claims pass))
              (term ()))
  )
(define-metafunction SP-core
  lift-claims : s -> d
  [(lift-claims s) (simplify-d (lift-claims-helper s))])
(define-metafunction SP-core
  simplify-d : d -> d
  ;; Remove later dynamic redeclaration because they are just mutation
  [(simplify-d (any_1 ... [x T] any_2 ... [x dynamic] any_3))
   (simplify-d (any_1 ... [x T] any_2 ... any_3))]
  [(simplify-d d) d])
(define-metafunction SP-core
  lift-claims-helper : s -> d
  [(lift-claims-helper (define/assign x t e)) ([x t])]
  [(lift-claims-helper (define/assign (attribute e_obj string_mem) t_mem e_mem))
   ()]
  [(lift-claims-helper (def x ([x_arg t_arg] ...) t_ret d_bdy s_bdy))
   ([x (def ([x_arg t_arg] ...) t_ret)])]
  [(lift-claims-helper (class x (t ...) m ...))
   ([x (class x (t ...) m ...)])]
  [(lift-claims-helper (if e_cnd s_thn s_els))
   (append (lift-claims-helper s_thn) (lift-claims-helper s_els))]
  [(lift-claims-helper (begin))
   ()]
  [(lift-claims-helper (begin s_1 s_2 ...))
   (append (lift-claims-helper s_1) (lift-claims-helper (begin s_2 ...)))]
  [(lift-claims-helper (delete x))
   ()]
  [(lift-claims-helper (delete (attribute e string)))
   ()]
  [(lift-claims-helper (return e))
   ()]
  [(lift-claims-helper (expr e))
   ()]
  [(lift-claims-helper pass)
   ()])

(module+ test
  (test-equal (term (method-name <)) (term "__lt__"))
  (test-equal (term (method-name >)) (term "__gt__"))
  (test-equal (term (method-name ==)) (term "__eq__"))
  (test-equal (term (method-name >=)) (term "__ge__"))
  (test-equal (term (method-name <=)) (term "__le__"))
  (test-equal (term (method-name +)) (term "__add__")))

(define-metafunction SP-core
  method-name : o -> string
  [(method-name <) "__lt__"]
  [(method-name >) "__gt__"]
  [(method-name ==) "__eq__"]
  [(method-name >=) "__ge__"]
  [(method-name <=) "__le__"]
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
              (term (bool-op or 0 1)))
  (test-equal (term (desugar-e (bin-op + 2 3)))
              (term ((attribute 2 "__add__") 3)))
  (test-equal (term (desugar-e (unary-op - 2)))
              (term ((attribute 2 "__neg__")))))

(define-metafunction SP-core
  desugar-e : e+ -> e
  [(desugar-e x) x]
  [(desugar-e c) c]
  [(desugar-e (tuple-syntax e+ ...))
   (tuple-syntax (desugar-e e+) ...)]
  [(desugar-e (set-syntax e+ ...))
   (set-syntax (desugar-e e+) ...)]
  [(desugar-e (dict-syntax [e+_key e+_val] ...))
   (dict-syntax [(desugar-e e+_key) (desugar-e e+_val)] ...)]
  [(desugar-e (is e+_1 e+_2))
   (is (desugar-e e+_1) (desugar-e e+_2))]
  [(desugar-e (is-not e+_1 e+_2))
   (is-not (desugar-e e+_1) (desugar-e e+_2))]
  [(desugar-e (if e+_cnd e+_thn e+_els))
   (if (desugar-e e+_cnd) (desugar-e e+_thn) (desugar-e e+_els))]
  [(desugar-e (attribute e+ string))
   (attribute (desugar-e e+) string)]
  [(desugar-e (e+_fun e+_arg ...))
   ((desugar-e e+_fun) (desugar-e e+_arg) ...)]
  [(desugar-e (reveal-type any ... e+))
   (reveal-type any ... (desugar-e e+))]
  ;; All below are interesting cases
  [(desugar-e (oc e+_1 e+_2))
   ((attribute (desugar-e e+_1) (method-name oc)) (desugar-e e+_2))]
  [(desugar-e (in e+_1 e+_2))
   ((attribute (desugar-e e+_2) "__contains__") (desugar-e e+_1))]
  [(desugar-e (subscript e+_1 e+_2))
   ((attribute (desugar-e e+_1) "__getitem__") (desugar-e e+_2))]
  [(desugar-e (bool-op ob e+))
   (desugar-e e+)]
  [(desugar-e (bool-op ob e+_1 e+_2 ...))
   (bool-op ob (desugar-e e+_1) (desugar-e (bool-op ob e+_2 ...)))]
  [(desugar-e (bin-op o2 e+_1 e+_2))
   ((attribute (desugar-e e+_1) (method-name o2)) (desugar-e e+_2))]
  [(desugar-e (unary-op - e+))
   ((attribute (desugar-e e+) "__neg__"))])

(module+ test
  (test-match SP-core s (term (desugar-s (define/assign (subscript x 2) dynamic 3))))
  (test-match SP-core s (term (desugar-s (def f ((c C)) dynamic
                                           (begin
                                             (define/assign (attribute c "x") dynamic 42)))))))
(define-metafunction SP-core
  desugar-s : s+ -> s
  [(desugar-s (class x (t+ ...) m+ ...))
   (class x ((desugar-t t+) ...) (desugar-m m+) ...)]
  [(desugar-s (def x ([x_arg t+_arg] ...) t+_ret s+))
   (def x ([x_arg (desugar-t t+_arg)] ...) (desugar-t t+_ret) (lift-claims s) s)
   (where s (desugar-s s+))]
  [(desugar-s (claim x e+))
   (claim x (desugar-e e+))]
  [(desugar-s (define/assign x t+ e+))
   (define/assign x (desugar-t t+) (desugar-e e+))]
  [(desugar-s (define/assign (attribute e+_map string_key) t+ e+_val))
   (define/assign (attribute (desugar-e e+_map) string_key) (desugar-t t+) (desugar-e e+_val))]
  ;; Interesting case
  [(desugar-s (define/assign (subscript e+_map e+_key) dynamic e+_val))
   (expr ((attribute e+_map "__setitem__") (desugar-e e+_val)))]
  [(desugar-s (return e+))
   (return (desugar-e e+))]
  [(desugar-s (if e+ s+_thn s+_els))
   (if (desugar-e e+) (desugar-s s+_thn) (desugar-s s+_els))]
  [(desugar-s (begin s+ ...))
   (begin (desugar-s s+) ...)]
  [(desugar-s pass)
   pass]
  [(desugar-s (delete x))
   (delete x)]
  [(desugar-s (delete (attribute e+ string)))
   (delete (attribute (desugar-e e+) string))]
  ;; Interesting case
  [(desugar-s (delete (subscript e+_map e+_key)))
   (expr ((attribute (desugar-e e+_map) "__delitem__") (desugar-e e+_key)))]
  [(desugar-s (expr e+))
   (expr (desugar-e e+))]
  )

(define-metafunction SP-core
  desugar-m : m+ -> m
  [(desugar-m (field string t+))
   (field string t+)]
  [(desugar-m (method string x_slf ([x_arg t+_arg] ...) t+_ret s+))
   (method string x_slf ([x_arg (desugar-t t+_arg)] ...) (desugar-t t+_ret) (lift-claims s) s)
   (where s (desugar-s s+))])

(define-metafunction SP-core
  desugar-t : t+ -> t
  [(desugar-t dynamic) dynamic]
  [(desugar-t None) None]
  [(desugar-t (subscript x (tuple-syntax t+ ...)))
   ((attribute x "__getitem__") (tuple-syntax (desugar-t t+) ...))]
  [(desugar-t (subscript x t+))
   ((attribute x "__getitem__") (desugar-t t+))]
  [(desugar-t (or-syntax t+_lft t+_rht))
   (or-syntax (desugar-t t+_lft) (desugar-t t+_rht))]
  [(desugar-t string) ,(string->symbol (term string))]
  [(desugar-t x) x])

(define-metafunction SP-core
  desugar-program : program+ -> program
  [(desugar-program (import-type ... s+ ...))
   (import-type ... (lift-claims s) s)
   (where s (desugar-s (begin s+ ...)))])
