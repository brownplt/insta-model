#lang racket
(require redex/reduction-semantics)
(require "grammar.rkt")
(provide (all-defined-out))

(define-extended-language SP-core SP

  ;; program
  (program level)

  ;; expressions
  (e x
     (con c)
     (tuple (e ...))
     (set (e ...))
     (dict ([e e] ...))
     (not e)
     (ob e e)
     (is e e)
     (if-exp e e e)
     (attribute e x)
     (call e (e ...))
     (lambda ([x t] ...) e))

  ;; type expression
  (t dynamic e)

  ;; statements
  (s (expr e)
     (return e)
     (assert e)
     (begin s ...)
     (if e s s)
     (delete x)
     (delete (attribute e x))
     (ann x t)
     (ann (attribute e x) t)
     (ann-assign x t e)
     (ann-assign (attribute e x) t e)
     (function-def x ([x t] ...) t level)
     (class x (e ...) (m ...))
     ;; import should only appear at the global scope
     (import-from x x))

  ;; class members
  (m (field x t)
     (field x t e)
     (method x ([x t] ...) t level))

  ;; (scope) level
  (level (local ([x d] ...) s))

  ;; rhs of declaration
  (d t
     (function-def (t ...) t)
     (class (e ...) ([x d] ...))
     (import-from x x))
  (import-d (import-from x x))
  (class-d (class (e ...) ([x d] ...)))
  (other-d t
           (function-def (t ...) t))

  ;; "binary" operators that can be replaced with names,
  ;;   which includes every o2 and some of oc
  (o o2
     < > == <= >=)

  ;; utilities
  (xd [x d])
  (xd* ([x d] ...))
  )


;; The remaining part of this file describe the desugaring process.
;; Desugaring is context-insensitive "__ __" doesn't rely on the type of
;; terms. This process remove three kinds of constructs:
;;   - operators (except is because they are primitive)
;;   - subscription
;;   - restrict assignment targets to variables and attributes
;; And lift variable declaration to the beginning of each block

(module+ test
  (test-equal (term (append (x) (y z)))
              (term (x y z))))
(define-metafunction SP-core
  append : (any ...) (any ...) -> (any ...)
  [(append (any_1 ...) (any_2 ...))
   (any_1 ... any_2 ...)])


(module+ test
  (test-equal (term (desugar-e "xyz"))
              (term "xyz"))
  (test-equal (term (desugar-e (con 42)))
              (term (con 42)))
  (test-equal (term (desugar-e (tuple ((con 2) (con 3)))))
              (term (tuple ((con 2) (con 3)))))
  (test-equal (term (desugar-e (set ((con 2) (con 3)))))
              (term (set ((con 2) (con 3)))))
  (test-equal (term (desugar-e (dict ([(con 2) (con 3)]))))
              (term (dict ([(con 2) (con 3)]))))
  (test-equal (term (desugar-e (if-exp (con 1) (con 2) (con 3))))
              (term (if-exp (con 1) (con 2) (con 3))))
  (test-equal (term (desugar-e (attribute "int" "__add__")))
              (term (attribute "int" "__add__")))
  (test-equal (term (desugar-e (call (con 1) [(con 2) (con 3)])))
              (term (call (con 1) [(con 2) (con 3)])))
  (test-equal (term (desugar-e (subscript (con 2) (con 3))))
              (term (call (attribute (con 2) "__getitem__") ((con 3)))))
  (test-equal (term (desugar-e (bin-op + (con 2) (con 3))))
              (term (call (attribute (con 2) "__add__") ((con 3)))))
  (test-equal (term (desugar-e (bool-op and [(con 2) (con 3) (con 4)])))
              (term (and (con 2) (and (con 3) (con 4)))))
  (test-equal (term (desugar-e (unary-op - (con 2))))
              (term (call (attribute (con 2) "__neg__") ())))
  (test-equal (term (desugar-e (compare (con 1) ([< (con 2)] [>= (con 3)]))))
              (term (and (call (attribute (con 1) "__lt__") ((con 2)))
                         (call (attribute (con 2) "__ge__") ((con 3)))))))

(define-metafunction SP-core
  desugar-e : e+ -> e
  [(desugar-e x) x]
  [(desugar-e (con c)) (con c)]
  [(desugar-e (tuple (e+ ...)))
   (tuple ((desugar-e e+) ...))]
  [(desugar-e (set (e+ ...)))
   (set ((desugar-e e+) ...))]
  [(desugar-e (dict ([e+_key e+_val] ...)))
   (dict ([(desugar-e e+_key) (desugar-e e+_val)] ...))]
  [(desugar-e (if-exp e+_cnd e+_thn e+_els))
   (if-exp (desugar-e e+_cnd) (desugar-e e+_thn) (desugar-e e+_els))]
  [(desugar-e (attribute e+ x))
   (attribute (desugar-e e+) x)]
  [(desugar-e (call e+_fun (e+_arg ...)))
   (call (desugar-e e+_fun) ((desugar-e e+_arg) ...))]
  [(desugar-e (subscript e+_1 e+_2))
   (desugar-subscript (desugar-e e+_1) (desugar-e e+_2))]
  [(desugar-e (bin-op o2 e+_1 e+_2))
   (call (attribute (desugar-e e+_1) (method-name o2)) ((desugar-e e+_2)))]
  [(desugar-e (bool-op ob (e+ ...)))
   (desugar-bool-op ob ((desugar-e e+) ...))]
  [(desugar-e (unary-op o1 e+))
   (desugar-unary-op o1 (desugar-e e+))]
  [(desugar-e (compare e+_lft ([oc e+_rht] ...)))
   (desugar-compare (desugar-e e+_lft) ([oc (desugar-e e+_rht)] ...))]
  [(desugar-e (lambda ([x t+] ...) e+))
   (lambda ([x (desugar-t t+)] ...) (desugar-e e+))])
(define-metafunction SP-core
  desugar-subscript : e e -> e
  [(desugar-subscript e_map e_key)
   (call (attribute e_map "__getitem__") (e_key))])
(define-metafunction SP-core
  desugar-bool-op : ob (e ...) -> e
  [(desugar-bool-op ob (e))
   e]
  [(desugar-bool-op ob (e_1 e_2 ...))
   (ob e_1 (desugar-bool-op ob (e_2 ...)))])
(define-metafunction SP-core
  desugar-unary-op : o1 e -> e
  [(desugar-unary-op - e)
   (call (attribute e "__neg__") ())]
  [(desugar-unary-op not e)
   (not e)])
(define-metafunction SP-core
  desugar-compare : e ([oc e] ...) -> e
  [(desugar-compare e_0 ([oc_1 e_1]))
   (desugar-oc oc_1 e_0 e_1)]
  [(desugar-compare e_0 ([oc_1 e_1] [oc_2 e_2] ...))
   (and (desugar-oc oc_1 e_0 e_1)
        (desugar-compare e_1 ([oc_2 e_2] ...)))])
(define-metafunction SP-core
  desugar-oc : oc e e -> e
  [(desugar-oc in e_1 e_2)
   (call (attribute e_2 "__contains__") (e_1))]
  [(desugar-oc not-in e_1 e_2)
   (not (desugar-oc in e_1 e_2))]
  [(desugar-oc is e_1 e_2)
   (is e_1 e_2)]
  [(desugar-oc is-not e_1 e_2)
   (not (desugar-oc is e_1 e_2))]
  [(desugar-oc oc e_1 e_2)
   (call (attribute e_1 (method-name oc)) (e_2))])


(module+ test
  (test-equal (term (method-name <)) (term "__lt__"))
  (test-equal (term (method-name >)) (term "__gt__"))
  (test-equal (term (method-name ==)) (term "__eq__"))
  (test-equal (term (method-name >=)) (term "__ge__"))
  (test-equal (term (method-name <=)) (term "__le__"))
  (test-equal (term (method-name +)) (term "__add__"))
  (test-equal (term (method-name -)) (term "__sub__"))
  (test-equal (term (method-name *)) (term "__mul__"))
  (test-equal (term (method-name /)) (term "__div__"))
  (test-equal (term (method-name bit-or)) (term "__or__")))
(define-metafunction SP-core
  method-name : o -> x
  [(method-name <) "__lt__"]
  [(method-name >) "__gt__"]
  [(method-name ==) "__eq__"]
  [(method-name >=) "__ge__"]
  [(method-name <=) "__le__"]
  [(method-name +) "__add__"]
  [(method-name -) "__sub__"]
  [(method-name *) "__mul__"]
  [(method-name /) "__div__"]
  [(method-name bit-or) "__or__"])

(module+ test
  (test-equal (term (desugar-t dynamic))
              (term dynamic))
  (test-equal (term (desugar-t (con None)))
              (term (con None))))
(define-metafunction SP-core
  desugar-t : t+ -> t
  [(desugar-t dynamic) dynamic]
  [(desugar-t e+) (desugar-e e+)])

(module+ test
  (test-equal (term (desugar-s pass))
              (term (begin)))
  (test-equal (term (desugar-s (expr (con 2))))
              (term (expr (con 2))))
  (test-equal (term (desugar-s (return (con 2))))
              (term (return (con 2))))
  (test-equal (term (desugar-s (assert "a")))
              (term (assert "a")))
  (test-equal (term (desugar-s (if "a" ((expr "b")) ((expr "c")))))
              (term (if "a" (begin (expr "b")) (begin (expr "c")))))
  (test-equal (term (desugar-s (delete "abc")))
              (term (delete "abc")))
  (test-equal (term (desugar-s (ann-assign "i" "int")))
              (term (ann "i" "int")))
  (test-equal (term (desugar-s (ann-assign "i" "int" "b")))
              (term (ann-assign "i" "int" "b")))
  (test-equal (term (desugar-s (assign "i" "b")))
              (term (ann-assign "i" dynamic "b")))
  (test-equal (term (desugar-s (aug-assign "i" + "b")))
              (term (ann-assign "i" dynamic (call (attribute "i" "__add__") ("b")))))
  (test-equal (term (desugar-s (class "C" ()
                                 ((function-def "foo" (["self" dynamic] ["i" "int"])
                                                dynamic
                                                ((assign "j" "i")
                                                 (return "i")))))))
              (term (class "C" ()
                      ((method "foo" (["self" dynamic] ["i" "int"])
                               dynamic
                               (local (["self" dynamic]
                                       ["i" "int"]
                                       ["j" dynamic])
                                 (begin
                                   (ann-assign "j" dynamic "i")
                                   (return "i"))))))))
  (test-equal (term (desugar-s (function-def "f" (["i" "int"]) dynamic ((return "i")))))
              (term (function-def "f" (["i" "int"]) dynamic (local (["i" "int"]) (begin (return "i"))))))
  (test-equal (term (desugar-s (import-from "__static__" ("PyDict" "CheckedDict"))))
              (term (begin (import-from "__static__" "PyDict")
                           (import-from "__static__" "CheckedDict"))))
  (test-equal (term (desugar-s (import-from "__static__" (*))))
              (term (begin
                      (import-from "__static__" "CheckedDict")
                      (import-from "__static__" "PyDict")
                      (import-from "__static__" "cast")))))
(define-metafunction SP-core
  make-begin : any ... -> any
  [(make-begin any_1 ... (begin any_2 ...) any_3 ...)
   (make-begin any_1 ... any_2 ... any_3 ...)]
  [(make-begin any ...)
   (begin any ...)])
(define-metafunction SP-core
  desugar-s : s+ -> s
  [(desugar-s pass)
   (begin)]
  [(desugar-s (expr e+))
   (expr (desugar-e e+))]
  [(desugar-s (return e+))
   (return (desugar-e e+))]
  [(desugar-s (assert e+))
   (assert (desugar-e e+))]
  [(desugar-s (if e+ (s+_thn ...) (s+_els ...)))
   (if (desugar-e e+)
       (make-begin (desugar-s s+_thn) ...)
       (make-begin (desugar-s s+_els) ...))]
  [(desugar-s (delete e+))
   (desugar-delete e+)]
  [(desugar-s (ann-assign e+_dst e+_ann))
   (desugar-ann e+_dst (desugar-t e+_ann))]
  [(desugar-s (ann-assign e+_dst t+_ann e+_src))
   (desugar-ann-assign e+_dst (desugar-t t+_ann) (desugar-e e+_src))]
  [(desugar-s (assign e+_dst e+_src))
   (desugar-ann-assign e+_dst dynamic (desugar-e e+_src))]
  [(desugar-s (aug-assign e+_dst o2 e+_src))
   (desugar-s (assign e+_dst (bin-op o2 e+_dst e+_src)))]
  [(desugar-s (class x (t+ ...) (s+ ...)))
   (class x ((desugar-t t+) ...) (m*-of-begin (make-begin (desugar-s s+) ...)))]
  [(desugar-s (function-def x ([x_arg t+_arg] ...) t+_ret (s+ ...)))
   (function-def x ([x_arg (desugar-t t+_arg)] ...) (desugar-t t+_ret)
                 (level-of-s
                  ([x_arg (desugar-t t+_arg)] ...)
                  (make-begin
                   (desugar-s s+)
                   ...)))]
  [(desugar-s (import-from x_mod (x_dst ...)))
   (make-begin (import-from x_mod x_dst) ...)]
  [(desugar-s (import-from x_mod (*)))
   (desugar-import-from-* x_mod)])
(module+ test
  (test-equal (term (desugar-import-from-* "__static__"))
              (term (begin
                      (import-from "__static__" "CheckedDict")
                      (import-from "__static__" "PyDict")
                      (import-from "__static__" "cast")))))
(define-metafunction SP-core
  desugar-import-from-* : x -> s
  [(desugar-import-from-* "__static__")
   (desugar-s (import-from "__static__" ("CheckedDict" "PyDict" "cast")))])
(module+ test
  (test-equal (term (desugar-delete "x"))
              (term (delete "x")))
  (test-equal (term (desugar-delete (attribute "obj" "x")))
              (term (delete (attribute "obj" "x"))))
  (test-equal (term (desugar-delete (subscript "lst" (con 2))))
              (term (expr (call (attribute "lst" "__delitem__") ((con 2)))))))
(define-metafunction SP-core
  desugar-delete : e+ -> s
  [(desugar-delete x)
   (delete x)]
  [(desugar-delete (attribute e+ x))
   (delete (attribute (desugar-e e+) x))]
  [(desugar-delete (subscript e+_map e+_key))
   (expr (call (attribute (desugar-e e+_map) "__delitem__") ((desugar-e e+_key))))])
(module+ test
  (test-equal (term (desugar-ann "i" "int"))
              (term (ann "i" "int")))
  (test-equal (term (desugar-ann (attribute "obj" "i") "int"))
              (term (ann (attribute "obj" "i") "int"))))
(define-metafunction SP-core
  desugar-ann : e+ t -> s
  [(desugar-ann x t)
   (ann x t)]
  [(desugar-ann (attribute e+ x) t)
   (ann (attribute (desugar-e e+) x) t)])
(module+ test
  (test-equal (term (desugar-ann-assign "i" "int" "abc"))
              (term (ann-assign "i" "int" "abc")))
  (test-equal (term (desugar-ann-assign (attribute "obj" "i") "int" "abc"))
              (term (ann-assign (attribute "obj" "i") "int" "abc")))
  (test-equal (term (desugar-ann-assign (subscript "lst" "i") dynamic "abc"))
              (term (expr (call (attribute "lst" "__setitem__") ("i" "abc")))))
  (test-equal (term (desugar-ann-assign (tuple ("i" "j")) dynamic "tpl"))
              (term (begin
                      (ann-assign "i" dynamic (call (attribute "tpl" "__getitem__") ((con 0))))
                      (ann-assign "j" dynamic (call (attribute "tpl" "__getitem__") ((con 1))))))))
(define-metafunction SP-core
  desugar-ann-assign : e+ t e -> s
  [(desugar-ann-assign x t e)
   (ann-assign x t e)]
  [(desugar-ann-assign (attribute e+_obj x_atr) t e_src)
   (ann-assign (attribute (desugar-e e+_obj) x_atr) t e_src)]
  [(desugar-ann-assign (subscript e+_map e_key) dynamic e_src)
   (expr (call (attribute (desugar-e e+_map) "__setitem__") (e_key e_src)))]
  [(desugar-ann-assign (tuple (e+_dst ...)) dynamic e_src)
   (make-begin
    (desugar-ann-assign e+_dst dynamic (desugar-subscript e_src (con number_src)))
    ...)
   (where (number_src ...) ,(range (length (term (e+_dst ...)))))])
(define-metafunction SP-core
  m*-of-begin : (begin s ...) -> (m ...)
  [(m*-of-begin (begin s ...))
   ((m-of-s s) ...)])
(module+ test
  (test-equal (term (m-of-s (ann "i" "int")))
              (term (field "i" "int")))
  (test-equal (term (m-of-s (ann-assign "i" "int" "abc")))
              (term (field "i" "int" "abc")))
  (test-equal (term (m-of-s (function-def "x" (["self" dynamic] ["a" "int"] ["b" "str"]) "dict"
                                          (local (["self" dynamic]
                                                  ["a" "int"]
                                                  ["b" "str"])
                                            (return (dict (["a" "b"])))))))
              (term (method "x" (["self" dynamic] ["a" "int"] ["b" "str"]) "dict"
                            (local (["self" dynamic]
                                    ["a" "int"]
                                    ["b" "str"])
                              (return (dict (["a" "b"]))))))))
(define-metafunction SP-core
  m-of-s : s -> m
  [(m-of-s (ann x t))
   (field x t)]
  [(m-of-s (ann-assign x t e))
   (field x t e)]
  [(m-of-s (function-def x ([x_arg t_arg] ...) t_ret level_bdy))
   (method x ([x_arg t_arg] ...) t_ret level_bdy)])

(define-metafunction SP-core
  level-of-s : ([x d] ...) s -> level
  [(level-of-s xd* s)
   (local (drop-later-dynamic (append xd* (xd*-of-s s))) s)])
(module+ test
  (test-equal (term (xd*-of-s (expr "abc")))
              (term ()))
  (test-equal (term (xd*-of-s (return "a")))
              (term ()))
  (test-equal (term (xd*-of-s (assert "a")))
              (term ()))
  (test-equal (term (xd*-of-s (begin)))
              (term ()))
  (test-equal (term (xd*-of-s (if "abc" (begin) (begin))))
              (term ()))
  (test-equal (term (xd*-of-s (delete "i")))
              (term ()))
  (test-equal (term (xd*-of-s (delete (attribute "obj" "a"))))
              (term ()))
  (test-equal (term (xd*-of-s (ann "i" "int")))
              (term (["i" "int"])))
  (test-equal (term (xd*-of-s (ann (attribute "abc" "i") "int")))
              (term ()))
  (test-equal (term (xd*-of-s (ann-assign "abc" dynamic "foo")))
              (term (["abc" dynamic])))
  (test-equal (term (xd*-of-s (ann-assign (attribute "self" "a") dynamic "abc")))
              (term ()))
  (test-equal (term (xd*-of-s (function-def "f" (["a" "int"]) "str" (local () (begin)))))
              (term (["f" (function-def ("int") "str")])))
  (test-equal (term (xd*-of-s (class "C" ("object")
                                ((field "i" "int")
                                 (field "s" "str" "hello")
                                 (method "greet" (["self" dynamic]) "str"
                                         (local ()
                                           (begin)))))))
              (term (["C" (class ("object")
                            (["i" "int"]
                             ["s" "str"]
                             ["greet" (function-def (dynamic) "str")]))])))
  (test-equal (term (xd*-of-s (import-from "__static__" "CheckedDict")))
              (term (["CheckedDict" (import-from "__static__" "CheckedDict")]))))
(define-metafunction SP-core
  xd*-of-s : s -> ([x d] ...)
  [(xd*-of-s (expr e))
   ()]
  [(xd*-of-s (return e))
   ()]
  [(xd*-of-s (assert e))
   ()]
  ;; interesting case!
  [(xd*-of-s (begin s ...))
   (xd*-of-s-begin (xd*-of-s s) ...)]
  [(xd*-of-s (if e_cnd s_thn s_els))
   (append (xd*-of-s s_thn) (xd*-of-s s_els))]
  [(xd*-of-s (delete x))
   ()]
  [(xd*-of-s (delete (attribute e x)))
   ()]
  ;; interesting case!
  [(xd*-of-s (ann x t))
   ([x t])]
  [(xd*-of-s (ann (attribute e x) t))
   ()]
  ;; interesting case!
  [(xd*-of-s (ann-assign x t e))
   ([x t])]
  [(xd*-of-s (ann-assign (attribute e_obj x_mem) t_mem e_src))
   ()]
  ;; interesting case!
  [(xd*-of-s (function-def x ([x_arg t_arg] ...) t_ret level_bdy))
   ([x (function-def (t_arg ...) t_ret)])]
  ;; interesting case!
  [(xd*-of-s (class x (e ...) (m ...)))
   ([x (class (e ...) ((xd-of-m m) ...))])]
  ;; interesting case!
  [(xd*-of-s (import-from x_mod x_var))
   ([x_var (import-from x_mod x_var)])])
(module+ test
  (test-equal (term (xd*-of-s-begin))
              (term ()))
  (test-equal (term (xd*-of-s-begin (["a" "int"]) (["b" "str"] ["c" "dict"])))
              (term (["a" "int"] ["b" "str"] ["c" "dict"]))))
(define-metafunction SP-core
  xd*-of-s-begin : ([x d] ...) ... -> ([x d] ...)
  [(xd*-of-s-begin)
   ()]
  [(xd*-of-s-begin xd*_1 xd*_2 ...)
   (append xd*_1 (xd*-of-s-begin xd*_2 ...))])
(module+ test
  (test-equal (term (xd-of-m (field "i" "int")))
              (term ["i" "int"]))
  (test-equal (term (xd-of-m (field "i" "int" "a")))
              (term ["i" "int"]))
  (test-equal (term (xd-of-m (method "f" () dynamic (local () (begin)))))
              (term ["f" (function-def () dynamic)])))
(define-metafunction SP-core
  xd-of-m : m -> [x d]
  [(xd-of-m (field x t))
   [x t]]
  [(xd-of-m (field x t e))
   [x t]]
  [(xd-of-m (method x ([x_arg t_arg] ...) t_ret level_bdy))
   [x (function-def (t_arg ...) t_ret)]])
(module+ test
  (test-equal (term (drop-later-dynamic ()))
              (term ()))
  (test-equal (term (drop-later-dynamic (["abc" "int"] ["abc" dynamic])))
              (term (["abc" "int"])))
  (test-equal (term (drop-later-dynamic (["abc" (function-def () "int")] ["abc" dynamic])))
              (term (["abc" (function-def () "int")]))))
(define-metafunction SP-core
  drop-later-dynamic : ([x d] ...) -> ([x d] ...)
  ;; Remove later dynamic redeclaration because they are just mutation
  [(drop-later-dynamic (xd_1 ... [x d] xd_2 ... [x dynamic] xd_3 ...))
   (drop-later-dynamic (xd_1 ... [x d] xd_2 ... xd_3 ...))]
  [(drop-later-dynamic xd*) xd*])

(module+ test
  (test-equal (term (desugar-program ()))
              (term (local () (begin)))))
(define-metafunction SP-core
  desugar-program : program+ -> program
  [(desugar-program (s+ ...))
   (level-of-s () (make-begin (desugar-s s+) ...))])