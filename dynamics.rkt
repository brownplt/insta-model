#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "desugar.rkt")
(require "compile.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-compiled
  ;; We extended global names to create heap labels
  (l .... number)
  ;; heaps map global names to heap allocated things
  (Σ ([number h] ...))
  ;; heap allocated things includes objects, and environments
  ;; the ☠ represents uninitialized variables
  (h object
     (env ([x v+☠] ...) l+☠))
  ;; According to Python's Data model: "every object has an identity,
  ;; a type and a value"
  ;; We don't need to store the identity here -- the heap Σ already did it.
  ;; But we still need a type (v), and an object value.
  ;; We actually need two things to represent an object values:
  ;;   - primitive values
  ;;   - key-attribute map
  (object (obj l g ρ))
  ;; Object values in the sense of Python's Data model.
  (g (con c)
     (tuple (v ...))
     (set (v ...))
     (dict ([v v] ...))
     (lambda l (x ...) s- level-)
     (class (v ...) ([x s-] ...))
     (prim-op l)
     (method v v)
     (nothing))
  ;; runtime expression involves applied functions
  (e- .... (deref v) (frame l s-))
  ;; runtime representation of programs
  (p [Σ l s-]
     (error)
     (terminate))
  ;; in expression reduce expression
  (ee hole
      (tuple (v ... ee e- ...))
      (set (v ... ee e- ...))
      (dict ([v v] ... [ee e-] [e- e-] ...))
      (dict ([v v] ... [v ee] [e- e-] ...))
      (is ee e-)
      (is v ee)
      (if-exp ee e- e-)
      (attribute mode ee x)
      (invoke-function l (v ... ee e- ...))
      (invoke-method l x ee (e- ...))
      (invoke-method l x v (v ... ee e- ...))
      (call-function ee (e- ...))
      (call-function v (v ... ee e- ...))
      (class (v ... ee e- ...) ([x s-+☠] ...))
      (frame l se))
  ;; in expression reduce statement
  (es (tuple (v ... es e- ...))
      (set (v ... es e- ...))
      (dict ([v v] ... [es e-] [e- e-] ...))
      (dict ([v v] ... [v es] [e- e-] ...))
      (is es e-)
      (is v es)
      (if-exp es e- e-)
      (attribute mode es x)
      (invoke-function l (v ... es e- ...))
      (invoke-method l x es (e- ...))
      (invoke-method l x v (v ... es e- ...))
      (call-function es (e- ...))
      (call-function v (v ... es e- ...))
      (class (v ... es e- ...) ([x s-+☠] ...))
      (frame l ss))
  ;; in statement reduce expression
  (se (expr ee)
      (return ee)
      (begin se s- ...)
      (if ee s- s-)
      (delete (attribute ee x))
      (assign x ee)
      (assign (attribute ee x) e-)
      (assign (attribute v x) ee))
  ;; in statement reduce statement
  (ss hole
      (expr es)
      (return es)
      (begin ss s- ...)
      (if es s- s-)
      (delete (attribute es x))
      (assign x es)
      (assign (attribute es x) e-)
      (assign (attribute v x) es))
  ;; builtin-op-l, a subset of l
  (builtin-op-l
   "isinstance"
   (attribute "int" "__add__")
   (attribute "int" "__sub__")
   (attribute "int" "__mul__")
   (attribute "int" "__div__")
   (attribute "dict" "__init__")
   (attribute "dict" "__getitem__")
   (attribute "dict" "__setitem__")
   (attribute "dict" "__delitem__")
   (attribute "CheckedDict" "__getitem__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__init__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__getitem__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__setitem__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__delitem__"))
  ;; utilities
  (v+☠ v ☠)
  (l+☠ l ☠))

(module+ test
  (test-equal (term (alloc ()))
              (term (())))
  (test-equal (term (alloc ()
                           (obj "int" (con 2) ())
                           (env () "builtin-env")))
              (term (([1 (env () "builtin-env")]
                      [0 (obj "int" (con 2) ())])
                     0
                     1))))
(define-metafunction SP-dynamics
  alloc : Σ h ... -> (Σ l ...)
  [(alloc Σ)
   (Σ)]
  [(alloc Σ h)
   ((extend Σ [l h]) l)
   (where l ,(length (term Σ)))]
  [(alloc Σ_1 h_1 h_2 ...)
   (Σ_3 l_1 l_2 ...)
   (where (Σ_2 l_1) (alloc Σ_1 h_1))
   (where (Σ_3 l_2 ...) (alloc Σ_2 h_2 ...))])

(module+ test
  (test-match SP-dynamics Σ (term (base-Σ))))
(define-metafunction SP-dynamics
  base-Σ : -> Σ
  [(base-Σ)
   ()])
(module+ test
  (test-equal (term
               (lookup-Σ ([0 (env () ☠)]) 0))
              (term
               (env () ☠)))
  (test-equal (term
               (lookup-Σ (base-Σ) "builtin-env"))
              (term
               (env
                (["object" (ref "object")]
                 ["int" (ref "int")]
                 ["bool" (ref "bool")]
                 ["str" (ref "str")]
                 ["dict" (ref "dict")]
                 ["set" (ref "set")]
                 ["type" (ref "type")]
                 ["isinstance" (ref "isinstance")]
                 ["len" (ref "len")]
                 ["Exception" (ref "Exception")]
                 ["max" (ref "max")]
                 ["min" (ref "min")]
                 ["issubclass" (ref "issubclass")])
                ☠))))
(define-metafunction SP-dynamics
  lookup-Σ : Σ l -> h
  [(lookup-Σ Σ number)
   (lookup Σ number)]
  [(lookup-Σ Σ l)
   (lookup-Σ-primitive l)])
(define-metafunction SP-dynamics
  lookup-Σ-primitive : l -> h
  [(lookup-Σ-primitive "builtin-env")
   (env ([x (ref x)] ...) ☠)
   (where ([x T] ...) (base-Γ))]
  [(lookup-Σ-primitive (con c))
   (obj (l-of-c c) (con c) ())]
  [(lookup-Σ-primitive string)
   (obj "type" (class ((ref l_sup) ...) ([x (expr (raise-error))] ...)) ([x l] ...))
   (where (class (l_sup ...) Γ ([x l] ...))
          (lookup-Ψ (base-Ψ) string))]
  [(lookup-Σ-primitive l)
   (obj "prim" (prim-op l) ())])

(module+ test
  (test-equal (term (update-env ([0 (env (["x" (ref "int")]) ☠)]) 0
                                "x" (ref "bool")))
              (term ([0 (env (["x" (ref "bool")]) ☠)])))
  (test-equal (term (update-env ([1 (env () 0)]
                                 [0 (env (["x" (ref "int")]) ☠)]) 1
                                                                  "x" (ref "bool")))
              (term ([1 (env () 0)]
                     [0 (env (["x" (ref "bool")]) ☠)]))))
(define-metafunction SP-dynamics
  update-env : Σ l x v+☠ -> Σ
  ;; if x is in the current environment
  [(update-env Σ l x v+☠_new)
   (update Σ [l (env (any_1 ... [x v+☠_new] any_2 ...) l+☠_out)])
   (where (env (any_1 ... [x v+☠_old] any_2 ...) l+☠_out) (lookup-Σ Σ l))]
  [(update-env Σ l x v)
   (update-env Σ l_out x v)
   (where (env any_map l_out) (lookup-Σ Σ l))])

(module+ test
  (test-equal (term (lookup-env ([0 (env (["x" (ref "int")]) ☠)])
                                0
                                "x"))
              (term (ref "int")))
  (test-equal (term (lookup-env ([1 (env () 0)]
                                 [0 (env (["x" (ref "int")]) ☠)])
                                1
                                "x"))
              (term (ref "int"))))
(define-metafunction SP-dynamics
  lookup-env : Σ l x -> v
  [(lookup-env Σ l x)
   (lookup (any_1 ... [x v+☠] any_2 ...) x)
   (where (env (any_1 ... [x v+☠] any_2 ...) l+☠_out) (lookup-Σ Σ l))]
  [(lookup-env Σ l x)
   (lookup-env Σ l_out x)
   (where (env any_map l_out) (lookup-Σ Σ l))])


(module+ test
  (test-equal (term (load (local ("i") (assign "i" (con 42)))))
              (term [([1 (env (["i" ☠]) 0)]
                      [0
                       (env
                        (["object" (ref "object")]
                         ["int" (ref "int")]
                         ["bool" (ref "bool")]
                         ["str" (ref "str")]
                         ["dict" (ref "dict")]
                         ["set" (ref "set")]
                         ["type" (ref "type")]
                         ["isinstance" (ref "isinstance")]
                         ["len" (ref "len")]
                         ["Exception" (ref "Exception")]
                         ["max" (ref "max")]
                         ["min" (ref "min")]
                         ["issubclass" (ref "issubclass")])
                        ☠)])
                     1
                     (assign "i" (con 42))])))
(define-metafunction SP-dynamics
  load : program- -> [Σ l s-]
  [(load (local (x ...) s-))
   [Σ_2 l_2 s-]
   (where ([x_builtin T] ...) (base-Γ))
   (where (Σ_1 l_1) (alloc (base-Σ) (env ([x_builtin (ref x_builtin)] ...) ☠)))
   (where (Σ_2 l_2) (alloc Σ_1 (env ([x ☠] ...) l_1)))])

(define-metafunction SP-dynamics
  delta : Σ l (v ...) -> [Σ e-]
  [(delta Σ (method "int" "__add__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(+ (term number_1) (term number_2))))]])

;; TODO: We should perform some checks here. do-invoke-function shouldn't raise errors.
(define-metafunction SP-dynamics
  do-call-function : Σ l h (v ...) -> [Σ l e-]
  [(do-call-function Σ l_env h (v ...))
   (do-invoke-function Σ l_env h (v ...))])

;; TODO: This is not quite right. The exact type is not necessarily l_cls
(define-metafunction SP-dynamics
  do-invoke-method : Σ l l x v (v ...) -> [Σ l e-]
  ;; invoke the method `x` declared in class `l`.
  [(do-invoke-method Σ l_cls x v_obj (v_arg ...))
   (do-invoke-function Σ l_env h_mth (v_obj v_arg ...))
   (where (obj "type" (class (v ...) ([x_mem s-_mem] ...)) ρ) (lookup-Σ Σ l_cls))
   (where l_mth (lookup ρ x))
   (where h_mth (lookup-Σ Σ l_mth))])

(define-metafunction SP-dynamics
  do-invoke-function : Σ l h (v ...) -> [Σ l e-]
  ;; invoke the function `h` under the environment `l`.
  [(do-invoke-function Σ l_env (obj "prim" (prim-op l) ()) (v ...))
   [Σ_1 l_env e-]
   (where [Σ_1 e-] (delta Σ l (v ...)))]
  [(do-invoke-function Σ_1 l_env
                       (obj "function"
                            (lambda l_1 (x_arg ...)
                              s-_chk
                              (local (x_var ...) s-_bdy))
                            ρ)
                       (v_arg ...))
   [Σ_2
    l_2
    (frame l_env
      (begin
        (assign x_arg v_arg)
        ...
        s-_chk
        s-_bdy))]
   (where (Σ_2 l_2) (alloc Σ_1 (env ([x_var ☠] ...) l_1)))])

(define-metafunction SP-dynamics
  do-attribute : mode h x -> v
  [(do-attribute mode (obj l g ρ) x)
   (ref (lookup ρ x))])

(define-syntax-rule (m? x y)
  (redex-match? SP-dynamics x y))

(module+ test
  (test-equal (term (do-is (ref (con 1)) (ref (con 2))))
              (term #f))
  (test-equal (term (do-is (ref (con 2)) (ref (con 2))))
              (term #t)))
(define-metafunction SP-dynamics
  do-is : v v -> boolean
  [(do-is v v) #t]
  [(do-is v_1 v_2) #f])
(module+ test
  (test-equal (term #t) (term (falsy? (obj "NoneType" (con None) ()))))
  (test-equal (term #t) (term (falsy? (obj "int" (con 0) ()))))
  (test-equal (term #t) (term (falsy? (obj "bool" (con #f) ()))))
  (test-equal (term #t) (term (falsy? (obj "tuple" (tuple ()) ()))))
  (test-equal (term #t) (term (falsy? (obj "set" (set ()) ()))))
  (test-equal (term #t) (term (falsy? (obj "dict" (dict ()) ()))))
  (test-equal (term #f) (term (falsy? (obj "int" (con 2) ()))))
  (test-equal (term #f) (term (falsy? (obj "bool" (con #t) ()))))
  (test-equal (term #f) (term (falsy? (obj "tuple" (tuple ((ref (con None)))) ())))))
(define-metafunction SP-dynamics
  falsy? : h -> boolean
  [(falsy? (obj "NoneType" (con None) ())) #t]
  [(falsy? (obj "int" (con 0) ())) #t]
  [(falsy? (obj "bool" (con #f) ())) #t]
  [(falsy? (obj "tuple" (tuple ()) ())) #t]
  [(falsy? (obj "set" (set ()) ())) #t]
  [(falsy? (obj "dict" (dict ()) ())) #t]
  [(falsy? h) #f])
(module+ test
  (test-equal (term (do-if-exp (obj "int" (con 1) ()) "x" "y"))
              (term "x"))
  (test-equal (term (do-if-exp (obj "int" (con 0) ()) "x" "y"))
              (term "y")))
(define-metafunction SP-dynamics
  do-if-exp : h e- e- -> e-
  [(do-if-exp h e-_thn e-_els)
   e-_els
   (where #t (falsy? h))]
  [(do-if-exp h e-_thn e-_els)
   e-_thn])


(define-metafunction SP-dynamics
  do-assign-attribute : Σ mode l x v -> Σ
  [(do-assign-attribute Σ mode l_obj x_mem (ref l_new))
   (update Σ [l_obj (obj l_cls g (update ρ [x_mem l_new]))])
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  do-delete-attribute : Σ mode l x -> Σ
  [(do-delete-attribute Σ mode l_obj x_mem)
   (update Σ [l_obj (obj l_cls g (any_1 ... any_2 ...))])
   (where (obj l_cls g (any_1 ... [x_mem l_mem] any_2 ...)) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  do-if : h s- s- -> s-
  [(do-if h s-_thn s-_els)
   s-_els
   (where #t (falsy? h))]
  [(do-if h s-_thn s-_els)
   s-_thn])

(module+ test
  (test-->> red-p
            (term (load (local ("i") (assign "i" (con 42)))))
            (term (terminate)))
  (test-->> red-p
            (term (load (local () (expr (con 2)))))
            (term (terminate)))
  (test-->> red-p
            (term (load (local ("i") (begin (assign "i" (con 42)) (expr "i")))))
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]) 0 (expr (ref (con 2)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]) 0 (begin (begin (expr (con 2))) (expr (con 3)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]) 0 (if (ref (con 0))
                                          (expr (con 2))
                                          (expr (con 3)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env (["abc" (ref (con 2))]) ☠)]) 0 (delete "abc")])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]
                    [1 (obj "MyClass" (nothing) (["abc" (con 2)]))])
                   0
                   (delete (attribute fast (ref 1) "abc"))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env (["abc" (ref (con 2))]) ☠)])
                   0
                   (assign "abc" (ref (con 3)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]
                    [1 (obj "MyClass" (nothing) (["abc" (con 2)]))])
                   0
                   (assign (attribute fast (ref 1) "abc") (ref (con 3)))])
            (term (terminate))))
(define red-p
  (reduction-relation
   SP-dynamics
   #:domain p
   ;; base case
   [--> [Σ l (begin)]
        (terminate)
        "terminate"]
   ;; return
   [--> (in-hole [Σ l_0 se] (in-hole (frame l_1 ss) (return v)))
        (in-hole [Σ l_1 se] v)
        "return"]
   ;; reduce statements
   [--> (in-hole [Σ l ss] (expr v))
        (in-hole [Σ l ss] (begin))
        "expr"]
   [--> (in-hole [Σ l ss] (if (ref l_cnd) s-_thn s-_els))
        (in-hole [Σ l ss] (do-if (lookup-Σ Σ l_cnd) s-_thn s-_els))
        "if"]
   [--> (in-hole [Σ_1 l ss] (delete x))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (update-env Σ_1 l x ☠))
        "delete"]
   [--> (in-hole [Σ_1 l ss] (delete (attribute mode (ref l_obj) x)))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (do-delete-attribute Σ_1 mode l_obj x))
        "delete attribute"]
   [--> (in-hole [Σ_1 l ss] (assign x_var v_new))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (update-env Σ_1 l x_var v_new))
        "assign"]
   [--> (in-hole [Σ_1 l ss] (assign (attribute mode (ref l_obj) x_mem) v_new))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (do-assign-attribute Σ_1 mode l_obj x_mem v_new))
        "assign attribute"]
   [--> (in-hole [Σ_1 l_env ss] (import-from x_mod x_var))
        (in-hole [Σ_2 l_env ss] (begin))
        (where (Type (subof l)) (T-of-import (import-from x_mod x_var)))
        (where Σ_2 (update-env Σ_1 l_env x_var (ref l)))
        "import-from"]
   [--> (in-hole [Σ l_env ss] (begin (begin) s- ...))
        (in-hole [Σ l_env ss] (begin s- ...))
        "begin"]
   ;; reduce expression
   [--> (in-hole [Σ l_env se] x)
        (in-hole [Σ l_env se] v)
        (where v (lookup-env Σ l_env x))
        "variable"]
   [--> (in-hole [Σ l_env se] (con c))
        (in-hole [Σ l_env se] (ref (con c)))
        "constant"]
   [--> (in-hole [Σ_1 l_env se] (tuple (v ...)))
        (in-hole [Σ_2 l_env se] (ref l))
        (where [Σ_2 l] (alloc Σ_1 (obj "tuple" (tuple (v ...)) ())))
        "tuple"]
   [--> (in-hole [Σ_1 l_env se] (set (v ...)))
        (in-hole [Σ_2 l_env se] (ref l)) 
        (where [Σ_2 l] (alloc Σ_1 (obj "set" (set (v ...)) ())))
        "set"]
   [--> (in-hole [Σ_1 l_env se] (dict ([v_key v_val] ...)))
        (in-hole [Σ_2 l_env se] (ref l))
        (where [Σ_2 l] (alloc Σ_1 (obj "dict" (dict ([v_key v_val] ...)) ())))
        "dict"]
   [--> (in-hole [Σ l_env se] (is v_1 v_2))
        (in-hole [Σ l_env se] (ref (con (do-is v_1 v_2))))
        "is"]
   [--> (in-hole [Σ l_env se] (if-exp (ref l) e-_thn e-_els))
        (in-hole [Σ l_env se] (do-if-exp (lookup-Σ Σ l) e-_thn e-_els))
        "if-exp"]
   [--> (in-hole [Σ l_env se] (attribute mode (ref l) x))
        (in-hole [Σ l_env se] (do-attribute mode (lookup-Σ Σ l) x))
        "attribute"]
   [--> (in-hole [Σ_1 l_env se] (lambda (x ...) s- level-))
        (in-hole [Σ_2 l_env se] (ref l_fun))
        (where [Σ_2 l_fun] (alloc Σ_1 (obj "function" (lambda l_env (x ...) s- level-) ())))
        "lambda"]
   [--> (in-hole [Σ_1 l_env se] (class (v ...) ([x s-] ...)))
        (in-hole [Σ_2 l_env se] (ref l_cls))
        (where [Σ_2 l_cls] (alloc Σ_1 (obj "type" (class (v ...) ([x s-] ...)) ())))
        "class"]
   [--> (in-hole [Σ_0 l_env0 se] (invoke-function l (v ...)))
        (in-hole [Σ_1 l_env1 se] e-)
        (where [Σ_1 l_env1 e-] (do-invoke-function Σ_0 l_env0 (lookup-Σ Σ_0 l) (v ...)))
        "invoke-function"]
   [--> (in-hole [Σ_0 l_env0 se] (invoke-method l x v_obj (v_arg ...)))
        (in-hole [Σ_1 l_env1 se] e-)
        (where [Σ_1 l_env1 e-] (do-invoke-method Σ_0 l_env0 l x v_obj (v_arg ...)))
        "invoke-method"]
   [--> (in-hole [Σ_0 l_env0 se] (call-function (ref l_fun) (v_arg ...)))
        (in-hole [Σ_1 l_env1 se] e-)
        (where [Σ_1 l_env1 e-] (do-call-function Σ_0 l_env0 (lookup-Σ Σ_0 l_fun) (v_arg ...)))
        "call-function"]))

(define-metafunction SP-dynamics
  calc : program- -> p
  [(calc program-)
   p
   (where (p) ,(apply-reduction-relation* red-p (term (load program-))))])

(define-metafunction SP-dynamics
  trace-calc : program- -> any
  [(trace-calc program-)
   ,(traces red-p (term (load program-)))])

(define-syntax-rule (test-run expect e)
  (test-match SP-dynamics expect (term (calc (compile-program (desugar-program e))))))

(define-syntax-rule (trace-run expect e)
  (traces red-p (term (load (compile-program (desugar-program e))))))

(test-run
 (terminate)
 ((import-from "__static__" ("PyDict"))))
(test-run
 (terminate)
 ((expr (con 42))))
(test-run
 (terminate)
 ((expr (bin-op + (con 2) (con 3)))))
(test-run
 (terminate)
 ((function-def "f" (["x" dynamic])
                dynamic
                ((return "x")))
  (expr (call "f" ((con 2))))))