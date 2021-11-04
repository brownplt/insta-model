#lang racket
(require redex)
(require redex-abbrevs)
(require "desugar.rkt")
(require "statics-basic-definitions.rkt")
(require "statics-utilities.rkt")
(require "statics-meaning-of-types.rkt")
(provide (all-defined-out))
(provide (all-from-out "statics-basic-definitions.rkt"))
(provide (all-from-out "statics-utilities.rkt"))
(provide (all-from-out "statics-meaning-of-types.rkt"))

(module+ test
  (test-equal (term (exist-incompatible-overrides? (base-Ψ)
                                                   ((["x" (instancesof "int")])
                                                    (["x" () (instancesof "int")]))))
              #t)
  (test-equal (term (exist-incompatible-overrides? (base-Ψ)
                                                   ((["x" (instancesof "int")]
                                                     ["x" (instancesof "str")])
                                                    ())))
              #t)
  (test-equal (term (exist-incompatible-overrides? (base-Ψ)
                                                   (()
                                                    (["x" () (instancesof "str")]
                                                     ["x" () (instancesof "int")]))))
              #t)
  (test-equal
   (term (exist-incompatible-overrides? (base-Ψ) ((["x" (instancesof "int")]
                                                   ["x" dynamic])
                                                  ())))
   #t)
  (test-equal
   (term (exist-incompatible-overrides? (base-Ψ) ((["x" (instancesof "int")]
                                                   ["x" (instancesof "int")])
                                                  ())))
   #f))
(define-metafunction SP-statics
  exist-incompatible-overrides? : Ψ flat-class -> boolean
  ;; a member name is both bound to a field and a method
  [(exist-incompatible-overrides? Ψ ((any_0 ... (string any_fieldspec ...) any_1 ...)
                                     (any_1 ... (string any_methodspec ...) any_2 ...)))
   #t]
  ;; a field is declared twice
  [(exist-incompatible-overrides? Ψ ((any_0 ... (string T_0) any_1 ... (string T_1) any_2 ...)
                                     any_methods))
   #t
   (where #t (≠ T_0 T_1))]
  ;; where a non-init method is overwritten in an incompatible way
  [(exist-incompatible-overrides? Ψ (any_fields
                                     (any_0 ...
                                      [string_0 ([x+☠_c T_arg1] ...) T_ret1]
                                      any_1 ...
                                      [string_0 ([x+☠_p T_arg2] ...) T_ret2]
                                      any_2 ...)))
   #t
   (where #f (Ψ⊢T≲T Ψ (-> ([x+☠_c T_arg1] ...) T_ret1) (-> ([x+☠_p T_arg2] ...) T_ret2)))
   (where #t (≠ string_0 "__init__"))]
  [(exist-incompatible-overrides? Ψ flat-class) #f])

(module+ test
  (check-judgment-holds*
   (⊢flat-class (base-Ψ) (() ()))
   (⊢flat-class (base-Ψ) ((["x" (instancesof "int")]) ())))
  (check-not-judgment-holds*
   (⊢flat-class (base-Ψ) ((["x" (instancesof "int")] ["x" (instancesof "str")]) ()))))
(define-judgment-form SP-statics
  #:mode (⊢flat-class I I)
  #:contract (⊢flat-class Ψ flat-class)

  ;; Is this flat-class well-formed under the class environment K?

  [(where #f (exist-incompatible-overrides? Ψ flat-class))
   -----------------
   (⊢flat-class Ψ flat-class)])

(define-judgment-form SP-statics
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_1 (base-Ψ))
   (where Γ_1 (base-Γ))
   (where Γ_2 (collect-imports Γ_1 import-type ...))
   ;; no redeclaration
   (where #f (some-redeclaration? d))
   (where (d_cls d_oth) (split-d d))
   ;; classes are defined first
   (where (Ψ_2 Γ_3) (define-classes Ψ_1 Γ_2 d_cls))
   ;; then we define other things, functions and ordinary varibles
   (where ([x_var D_var] ...) d_oth)
   (evalD Ψ_2 Γ_3 D_var T_var) ...
   (where Γ_4 (extend Γ_3 [x_var T_var] ...))
   ;; every class is well-formed
   (where ([cid C] ...) Ψ_2)
   (Ψ⊢cid Ψ_2 cid) ...
   ;; the module body is well-formed
   (ΨΓΓ⊢s⇐T+☠ Ψ_2 Γ_4 Γ_4 s ☠)
   ------------------------
   (⊢p (import-type ... d s))])

(module+ test
  (test-equal (term (split-d ([x int]
                              [y (class y (object)
                                   (field "x" int)
                                   (method "f" self () str () pass))]
                              [z (def ([arg1 int] [arg2 str]) None)])))
              (term (([y (class y (object)
                           (field "x" int)
                           (method "f" self () str () pass))])
                     ([x int]
                      [z (def ([arg1 int] [arg2 str]) None)])))))
(define-metafunction SP-statics
  split-d : d -> (d d)
  ;; categorize declarations into classes adn others
  [(split-d ()) (() ())]
  [(split-d ([x t] any ...))
   (d_cls (extend d_oth [x t]))
   (where (d_cls d_oth) (split-d (any ...)))]
  [(split-d ([x (def ([x_arg t_arg] ...) t_ret)] any ...))
   (d_cls (extend d_oth [x (def ([x_arg t_arg] ...) t_ret)]))
   (where (d_cls d_oth) (split-d (any ...)))]
  [(split-d ([x (class x (t ...) m ...)] any ...))
   ((extend d_cls [x (class x (t ...) m ...)]) d_oth)
   (where (d_cls d_oth) (split-d (any ...)))])

(module+ test
  (test-equal (term (collect-one-import (base-Γ) "__static__" "PyDict"))
              (term (extend (base-Γ) [PyDict (classitself "dict")])))
  (test-equal (term (collect-one-import (base-Γ) "__static__" "CheckedDict"))
              (term (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])))
  (test-equal (term (collect-one-import (base-Γ) "__static__" "cast"))
              (term (extend (base-Γ) [cast (type-op "cast")])))
  (test-equal (term (collect-one-import (base-Γ) "__static__" "inline"))
              (term (base-Γ)))
  (test-equal (term (collect-one-import (base-Γ) "typing" "Any"))
              (term (extend (base-Γ) [Any dynamic])))
  (test-equal (term (collect-one-import (base-Γ) "typing" "Final"))
              (term (extend (base-Γ) [Final (prim-generic "Final")])))
  (test-equal (term (collect-one-import (base-Γ) "typing" "Optional"))
              (term (extend (base-Γ) [Optional (prim-generic "Optional")])))
  (test-equal (term (collect-one-import (base-Γ) "other_module" "whatever"))
              (term (extend (base-Γ) [whatever dynamic]))))
(define-metafunction SP-statics
  collect-one-import : Γ string_mod string_var -> any
  [(collect-one-import Γ "__static__" "PyDict")
   (extend Γ [PyDict (classitself "dict")])]
  [(collect-one-import Γ "__static__" "CheckedDict")
   (extend Γ [CheckedDict (prim-generic "CheckedDict")])]
  ;; cast
  [(collect-one-import Γ "__static__" "cast")
   (extend Γ [cast (type-op "cast")])]
  ;; skip inline
  [(collect-one-import Γ "__static__" "inline")
   Γ]
  ;; There are no more things from __static__ doesn't exist
  [(collect-one-import Γ "__static__" string)
   #f]
  ;; typing provides Any
  [(collect-one-import Γ "typing" "Any")
   (extend Γ [Any dynamic])]
  ;; typing provides Final
  [(collect-one-import Γ "typing" "Final")
   (extend Γ [Final (prim-generic "Final")])]
  ;; typing provides Optional
  [(collect-one-import Γ "typing" "Optional")
   (extend Γ [Optional (prim-generic "Optional")])]
  ;; everything else is dynamic
  [(collect-one-import Γ string_mod string_var)
   (extend Γ [,(string->symbol (term string_var)) dynamic])])

(module+ test
  (test-equal
   (term (collect-imports ()))
   (term ()))
  (test-equal
   (term (collect-imports () (import-from "__static__" ("PyDict"))))
   (term ([PyDict (classitself "dict")]))))
(define-metafunction SP-statics
  collect-imports : Γ import-type ... -> any  ;; actually (Ψ Γ) or #f
  [(collect-imports Γ) Γ]
  [(collect-imports Γ (import-from string ()) import-type ...)
   (collect-imports Γ import-type ...)]
  [(collect-imports
    Γ_1
    (import-from string_mod (string_var1 string_var2 ...))
    import-type ...)
   (collect-imports
    Γ_2
    (import-from string_mod (string_var2 ...))
    import-type ...)
   (where Γ_2 (collect-one-import Γ_1 string_mod string_var1))]
  [(collect-imports any ...) #f])

(module+ test
  (test-equal (term (some-redeclaration? ()))
              (term #f))
  (test-equal (term (some-redeclaration? ([x int] [x int])))
              (term #t))
  (test-equal (term (some-redeclaration? ([x int] [y int])))
              (term #f)))
(define-metafunction SP-statics
  some-redeclaration? : d -> boolean
  [(some-redeclaration? (any_1 ... [x any_fst] any_2 ... [x any_snd] any_3 ...))
   #t]
  [(some-redeclaration? d)
   #f])

(define-metafunction SP-statics
  compute-parent : T ... -> cid+dynamic+☠
  [(compute-parent) "object"]
  [(compute-parent (instancesof cid)) cid]
  [(compute-parent T ...) dynamic])

(module+ test
  (test-equal (term (define-classes (base-Ψ) (base-Γ) ()))
              (term ((base-Ψ) (base-Γ))))
  (test-equal (term (define-classes (base-Ψ) (base-Γ)
                      ([C (class C ())]
                       [D (class D ())])))
              (term ((extend (base-Ψ)
                             [1 (class D "object" () ())]
                             [0 (class C "object" () ())])
                     (extend (base-Γ)
                             [D (classitself 1)]
                             [C (classitself 0)]))))
  (test-equal (term (define-classes (base-Ψ) (base-Γ)
                      ([C (class C ())]
                       [D (class D (C))])))
              (term ((extend (base-Ψ)
                             [1 (class D 0 () ())]
                             [0 (class C "object" () ())])
                     (extend (base-Γ)
                             [D (classitself 1)]
                             [C (classitself 0)])))))
(define-metafunction SP-statics
  define-classes : Ψ Γ d -> (Ψ Γ)
  [(define-classes Ψ_1 Γ_1 d)
   (Ψ_3 Γ_2)
   (where (Ψ_2 Γ_2) (declare-classes Ψ_1 Γ_1 d))
   (where Ψ_3 (initialize-classes Ψ_2 Γ_2 d))])

(define-metafunction SP-statics
  declare-classes : Ψ Γ d -> (Ψ Γ)
  ;; claim the variables but don't initialize them. We need
  ;; to seperate the steps because classes can be recursive
  [(declare-classes Ψ Γ ()) (Ψ Γ)]
  [(declare-classes Ψ_1 Γ_1 ([x (class x any_cls ...)] any_clm ...))
   (declare-classes Ψ_2 Γ_2 (any_clm ...))
   (where (Ψ_2 cid) (Ψ-alloc Ψ_1 ☠))
   (where Γ_2 (extend Γ_1 [x (classitself cid)]))])

(define-metafunction SP-statics
  initialize-classes : Ψ Γ d -> Ψ
  ;; initialize the class variables.
  ;; The d is guarantee to contain just class declarations
  [(initialize-classes Ψ Γ ()) Ψ]
  [(initialize-classes Ψ_1 Γ ([x (class x (t_par ...) m ...)] any ...))
   (initialize-classes Ψ_2 Γ (any ...))
   (where (((string_fld t_fld) ...)
           ((string_mth ([x_arg t_arg] ...) t_ret) ...))
          (collect-mems m ...))
   (judgment-holds (evalo* Ψ_1 Γ (t_fld ...) (T_fld ...)))
   (judgment-holds (evalo** Ψ_1 Γ ((t_arg ...) ...) ((T_arg ...) ...)))
   (judgment-holds (evalo* Ψ_1 Γ (t_ret ...) (T_ret ...)))
   (judgment-holds (evalo* Ψ_1 Γ (t_par ...) (T_par ...)))
   (where C (class x (compute-parent T_par ...)
              ([string_fld T_fld] ...)
              ([string_mth ([x_arg T_arg] ...) T_ret] ...)))
   (where (classitself cid) (lookup Γ x))
   (where Ψ_2 (Ψ-init Ψ_1 cid C))])

(define-metafunction SP-statics
  collect-local-defs-and-clss : Ψ Γ s ... -> any
  ;; What are the classes and variables defined at the top level?
  [(collect-local-defs-and-clss Ψ Γ s ...)
   (collect-variables-helper Ψ Γ local () s ...)])

(define-judgment-form SP-statics
  #:mode (¬Ψ⊢Γ I I)
  #:contract (¬Ψ⊢Γ Ψ Γ)
  [(where #f (Ψ⊢T≲T Ψ T_new T_old))
   ---------------------
   (¬Ψ⊢Γ Ψ ([x_1 T_1] ... [x T_new] [x_2 T_2] ... [x T_old] [x_3 T_3] ... ))])

(define-judgment-form SP-statics
  #:mode (Ψ⊢Γ I I)
  #:contract (Ψ⊢Γ Ψ Γ)
  [(where #f (¬Ψ⊢Γ Ψ Γ))
   ---------------------
   (Ψ⊢Γ Ψ Γ)])

(define-judgment-form SP-statics
  #:mode (Ψ⊢cid I I)
  #:contract (Ψ⊢cid Ψ cid)
  ;; Is this class well-formed (not overriding member signatures incorrectly)?

  ;; no incompatible override
  [(where flat-class (flatten-class Ψ cid))
   (⊢flat-class Ψ flat-class)
   --------------------------------------
   (Ψ⊢cid Ψ cid)]
  )


(module+ test
  (test-equal (term (collect-mems (field "x" int)
                                  (method "f" self ([x int] [y str]) None () pass)))
              (term ((["x" int])
                     (["f" ([x int] [y str]) None])))))
(define-metafunction SP-statics
  collect-mems : m ... -> (([string t] ...)
                           ([string ([x t] ...) t] ...))
  ;; What are the fields and methods defined in this class?

  [(collect-mems) (()
                   ())]

  [(collect-mems (field string t) m ...)
   (((string t) any_field ...)
    (any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems m ...))]

  [(collect-mems
    (method string_method x_slf ([x_arg t_arg] ...) t_ret d s)
    m ...)
   ((any_field ...)
    ((string_method ([x_arg t_arg] ...) t_ret) any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems m ...))])

(module+ test
  (check-not-judgment-holds*
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (base-Γ)
              (base-Γ)
              (return 2)
              ☠))
  (check-judgment-holds*
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (extend (base-Γ) [x (instancesof "int")])
              (extend (base-Γ) [x (instancesof "int")])
              (define/assign x int 42)
              dynamic)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (base-Γ)
              (base-Γ)
              (def f ([x int]) None () pass)
              dynamic)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (base-Γ)
              (base-Γ)
              pass
              dynamic)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (base-Γ)
              (base-Γ)
              (class C ())
              dynamic)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (base-Γ)
              (base-Γ)
              (expr None)
              dynamic)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (base-Γ) (begin) ☠)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (base-Γ) (begin) (instancesof "None"))
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (base-Γ) (begin) dynamic)
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (base-Γ) (begin) ("optional" "int"))
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (base-Γ) (begin (return 42) (return "foo")) (instancesof "int"))
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")]) (extend (base-Γ) [x ("optional" "int")])
              (begin
                (if (is x None)
                    (begin (return 42))
                    (begin))
                (return x))
              (instancesof "int"))
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (extend (base-Γ) [x ("optional" "int")])
              (extend (base-Γ) [x ("optional" "int")])
              (begin
                (if (is-not x None)
                    (begin)
                    (begin (return 42)))
                (return x))
              (instancesof "int"))
   (ΨΓΓ⊢s⇐T+☠ (base-Ψ)
              (extend (base-Γ) [x ("optional" "int")] [y ("optional" "int")])
              (extend (base-Γ) [x ("optional" "int")] [y ("optional" "int")])
              (if (bool-op and (is-not x None) (is-not y None))
                  (begin (return ((attribute x "__add__") y)))
                  (begin (return 42)))
              (instancesof "float"))))

(define-judgment-form SP-statics
  #:mode (evalD I I I O)
  #:contract (evalD Ψ Γ D T)

  [(evalo Ψ Γ t T)
   ----------------------
   (evalD Ψ Γ t T)]

  [(evalo* Ψ Γ (t_arg ...) (T_arg ...))
   (evalo Ψ Γ t_ret T_ret)
   ----------------------
   (evalD Ψ Γ
          (def ([x_arg t_arg] ...) t_ret)
          (-> ([x_arg T_arg] ...) T_ret))])

(define-judgment-form SP-statics
  #:mode (ΨΓΓ⊢s⇐T+☠ I I I I I)
  #:contract (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ s T+☠)
  ;; DO INDUCTION ON S
  ;; Is the statement s well-formed under the type and class environment Γ?

  [------------------------ "claim"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (claim x t) _)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_1 T)
   (ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_2 T)
   ------------------------ "define/assign"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (define/assign e_1 t e_2) _)]

  [(evalo Ψ Γ_1 t_ret T_ret)
   (where d_2 (extend d_1 [x_arg t_arg] ...))
   (where d_3 (simplify-d d_2))
   (where #f (some-redeclaration? d_3))
   (where (([x_cls any] ...) ([x_var D_var] ...)) (split-d d_3))
   (evalD Ψ Γ_1 D_var T_var) ...
   (where Γ_2 (extend Γ_1 [x_cls dynamic] ... [x_var T_var] ...))
   ;; always use the persistent enrionment
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ_2 Γ_2 s T_ret)
   ------------------------ "def"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ_1 Γ_lcl (def x_fun ([x_arg t_arg] ...) t_ret d_1 s) _)]

  [(ΨΓt⊢m Ψ Γ x m) ...
   ------------------------ "static class"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (class x (t ...) m ...) _)]

  ;; If statements are handled elsewhe

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl x dynamic)
   ------------------------ "delete x"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (delete x) _)]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl (attribute e string) dynamic)
   ------------------------ "delete attribute"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (delete (attribute e string)) _)]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e dynamic)
   ------------------------ "expr"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (expr e) _)]

  [------------------------ "pass"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl pass _)]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e T)
   ------------------------ "return"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (return e) T)]

  [(ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl e s_thn s_els T+☠)
   --------------------------------------- "if"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (if e s_thn s_els) T+☠)]

  ;; begin-related

  [--------------------- "begin-end no expect"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin) ☠)]

  [(ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (return None) T)
   --------------------- "begin-end some expect"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin) T)]

  [(ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (return e) T+☠)
   --------------------- "begin-return"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin (return e) s ...) T+☠)]

  [(ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin s_1 ... s_2 ...) T+☠)
   --------------------- "begin-begin"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin (begin s_1 ...) s_2 ...) T+☠)]

  [(where T_tgt (lookup Γ x))
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e T_src)
   (Ψ⊢T≲T Ψ T_src T_tgt)
   (where T_lcl (intersection Ψ T_src T_tgt))
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ (extend Γ_lcl [x T_lcl]) (begin s ...) T+☠)
   --------------------- "begin-define/assign"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin (define/assign x t e) s ...) T+☠)]

  [(ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (if e (begin s_thn s ...) (begin s_els s ...)) T+☠)
   --------------------- "begin-if"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin (if e s_thn s_els) s ...) T+☠)]

  [(ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl s_1 T+☠)
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin s_2 ...) T+☠)
   (where #f ,(redex-match? SP-statics (define/assign x t e) (term s_1)))
   ----------------- "begin-otherwise"
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl (begin s_1 s_2 ...) T+☠)]
  )


(module+ test
  (check-judgment-holds*
   (ΨΓ⊢ifess⇐T+☠ (base-Ψ)
                 ([x ("optional" "int")])
                 ([x ("optional" "int")])
                 (is x None)
                 (return 42)
                 (return x)
                 (instancesof "int"))
   (ΨΓ⊢ifess⇐T+☠ (base-Ψ)
                 ([x ("optional" "int")])
                 ([x ("optional" "int")])
                 (is-not x None)
                 (return x)
                 (return 42)
                 (instancesof "int"))))
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢ifess⇐T+☠ I I I I I I I)
  #:contract (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ e s s T+☠)

  [(ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl (is x None) s_els s_thn T+☠)
   ------------------ "if is-not"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl (is-not x None) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl x T)
   (where T_thn (intersection Ψ T (instancesof "None")))
   (where T_els (remove-None T))
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ (extend Γ_lcl [x T_thn]) s_thn T+☠)
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ (extend Γ_lcl [x T_els]) s_els T+☠)
   ------------------ "if is"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl (is x None) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_1 (instancesof "bool"))
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl e_1 (if e_2 s_thn s_els) s_els T+☠)
   ------------------ "if and"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl (bool-op and e_1 e_2) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_1 (instancesof "bool"))
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl e_1 s_thn (if e_2 s_thn s_els) T+☠)
   ------------------ "if or"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl (bool-op or e_1 e_2) s_thn s_els T+☠)]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e dynamic)
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl s_thn T+☠)
   (ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ_lcl s_els T+☠)
   ------------------ "usual if"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ Γ_lcl e s_thn s_els T+☠)])

;; TODO merge the two if judgments
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢ifeee⇒T I I I I I I O)
  #:contract (ΨΓ⊢ifeee⇒T Ψ Γ Γ e e e T)

  [(ΨΓ⊢ifeee⇒T Ψ Γ Γ_lcl (is x None) e_els e_thn T)
   ------------------ "if is-not"
   (ΨΓ⊢ifeee⇒T Ψ Γ Γ_lcl (is-not x None) e_thn e_els T)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl x ("optional" cid))
   (ΨΓ⊢e⇒T Ψ Γ (extend Γ_lcl [x (instancesof "None")]) e_thn T_thn)
   (ΨΓ⊢e⇒T Ψ Γ (extend Γ_lcl [x (instancesof cid)]) e_els T_els)
   ------------------ "if is"
   (ΨΓ⊢ifeee⇒T Ψ Γ Γ_lcl (is x None) e_thn e_els (union Ψ T_thn T_els))]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_cnd dynamic)
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_thn T_thn)
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_els T_els)
   ------------------ "usual if"
   (ΨΓ⊢ifeee⇒T Ψ Γ Γ_lcl e_cnd e_thn e_els (union Ψ T_thn T_els))])


(define-judgment-form SP-statics
  #:mode (ΨΓt⊢m I I I I)
  #:contract (ΨΓt⊢m Ψ Γ t m)

  ;; Is this m well-formed under the environment Γ?

  [-------------------------
   (ΨΓt⊢m Ψ Γ t_slf (field string t))]

  [(ΨΓΓ⊢s⇐T+☠ Ψ Γ Γ (def whatever ([x_slf t_slf] [x_arg t_arg] ...) t_ret d s) ☠)
   -------------------------
   (ΨΓt⊢m Ψ Γ t_slf (method string_method x_slf ([x_arg t_arg] ...) t_ret d s))])

(module+ test
  (check-judgment-holds*
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) None (instancesof "None"))
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) #t (instancesof "int"))
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) 42 (instancesof "int"))
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) (dict-syntax) (instancesof "dict"))
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) (set-syntax) (instancesof "set"))
   (ΨΓΓ⊢e⇐T (base-Ψ) (base-Γ) (base-Γ) ((attribute (dict-syntax ("foo" 1)) "__getitem__") "foo") dynamic)
   (ΨΓΓ⊢e⇐T (base-Ψ)
            (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
            (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
            ((attribute CheckedDict "__getitem__") (tuple-syntax str int))
            (classitself ("CheckedDict" "str" "int")))
   (ΨΓΓ⊢e⇐T (base-Ψ)
            (extend (base-Γ) [d (instancesof ("CheckedDict" "str" "int"))])
            (extend (base-Γ) [d (instancesof ("CheckedDict" "str" "int"))])
            ((attribute d "__getitem__") "foo")
            (instancesof "int"))))


(define-judgment-form SP-statics
  #:mode (ΨΓΓ⊢e⇐T I I I I I)
  #:contract (ΨΓΓ⊢e⇐T Ψ Γ Γ e T)
  ;; Is expression e well-formed under Γ and usable as a t?

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e T_1)
   (Ψ⊢T≲T Ψ T_1 T_2)
   ------------------------ "switch"
   (ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e T_2)]
  )

(module+ test
  (check-judgment-holds*
   (as-fun (base-Ψ) (-> ([x (instancesof "int")]) (instancesof "str")) 1 (-> ([x (instancesof "int")]) (instancesof "str")))
   (as-fun (base-Ψ) (classitself "object") 0 (-> () (instancesof "object")))
   (as-fun (base-Ψ) (classitself "str") 1 (-> ([☠ dynamic]) (instancesof "str")))
   (as-fun (extend (base-Ψ) [0 (class MyClass "object" () ())])
           (classitself 0) 0 (-> () (instancesof 0)))
   (as-fun (extend (base-Ψ) [0 (class MyClass "str" () ())])
           (classitself 0) 1 (-> ([☠ dynamic]) (instancesof 0)))
   (as-fun (extend (base-Ψ) [0 (class MyClass "object" () (["__init__" ([x (instancesof "int")]) dynamic]))])
           (classitself 0) 1 (-> ([x (instancesof "int")]) (instancesof 0))))
  (check-not-judgment-holds*
   (as-fun (base-Ψ) (-> ([x dynamic]) dynamic) 0 any)
   (as-fun (base-Ψ) (-> ([x dynamic]) dynamic) 2 any)
   (as-fun (base-Ψ) (classitself "str") 2 any)))
(define-judgment-form SP-statics
  #:mode (as-fun I I I O)
  #:contract (as-fun Ψ T number T)

  [(constructor-ofo Ψ cid ([x+☠_arg T_arg] ...))
   (where #t (= (len ([x+☠_arg T_arg] ...)) number))
   ------------------------ "class-as-fun"
   (as-fun Ψ
           (classitself cid)
           number
           (-> ([x+☠_arg T_arg] ...) (instancesof cid)))]

  [(where #t (= (len (T_arg ...)) number))
   ------------------------ "fun-as-fun"
   (as-fun Ψ
           (-> ([x+☠_arg T_arg] ...) T_ret)
           number
           (-> ([x+☠_arg T_arg] ...) T_ret))]

  [(where (T_arg ...) ,(make-list (term number) (term dynamic)))
   (where T_ret dynamic)
   ------------------------ "dyn-as-fun"
   (as-fun Ψ dynamic number (-> ([☠ T_arg] ...) T_ret))]
  )

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) int (classitself "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) None (instancesof "None"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) #t (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) 42 (instancesof "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (set-syntax) (instancesof "set"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (set-syntax "foo" 42) (instancesof "set"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (dict-syntax) (instancesof "dict"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (dict-syntax [42 "foo"] ["bar" 120]) (instancesof "dict"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) ((attribute (dict-syntax ("foo" 1)) "__getitem__") "foo") dynamic)
   (ΨΓ⊢e⇒T (base-Ψ)
           (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           ((attribute CheckedDict "__getitem__") (tuple-syntax str int))
           (classitself ("CheckedDict" "str" "int")))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (attribute (dict-syntax) "__getitem__") (-> ([☠ dynamic]) dynamic))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (bool-op and #t #f) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (bool-op or #t #f) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (int "123") (instancesof "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (is "foo" None) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (base-Γ) (is-not 42 "x") (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ)
           (extend (base-Γ) [f (-> ([x (instancesof "int")]) (instancesof "str"))])
           (extend (base-Γ) [f (-> ([x (instancesof "int")]) (instancesof "str"))])
           (f 42)
           (instancesof "str"))))
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢e⇒T I I I I O)
  #:contract (ΨΓ⊢e⇒T Ψ Γ Γ e T)
  ;; Is expression e well-formed under type environment Γ and usable as a T?

  [----------------------- "None"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl None (instancesof "None"))]

  ;; TODO remove this
  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e T)
   (show (the-type-of e is T the context is Γ Γ_lcl at any ...))
   ----------------------- "reveal-type"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (reveal-type any ... e) T)]

  [----------------------- "integer"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl integer (instancesof "int"))]

  [(where #f ,(redex-match? SP-statics integer (term number)))
   ----------------------- "float"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl number (instancesof "float"))]

  [----------------------- "boolean"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl boolean (instancesof "bool"))]

  [----------------------- "string"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl string (instancesof "str"))]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_key dynamic) ...
   (ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_value dynamic) ...
   ----------------------- "dict"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (dict-syntax [e_key e_value] ...) (instancesof "dict"))]

  [(ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_elt dynamic) ...
   ----------------------- "set"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (set-syntax e_elt ...) (instancesof "set"))]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_2 T_2)
   ------------------------ "is"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (is e_1 e_2) (instancesof "bool"))]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_2 T_2)
   ------------------------ "is-not"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (is-not e_1 e_2) (instancesof "bool"))]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_2 T_2)
   ------------------------ "bool-op"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (bool-op ob e_1 e_2) (union Ψ T_1 T_2))]

  [(where ((attribute x "__getitem__") (tuple-syntax t ...)) e)
   (evalo Ψ Γ e (instancesof cid))
   ------------------------ "generic"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e (classitself cid))]

  [(lookupo Γ_lcl x T)
   ------------------------ "variable"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl x T)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_ins T_ins)
   (where T_mem (lookup-member Ψ T_ins string_mem))
   ----------------------- "attribute / access member"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (attribute e_ins string_mem) T_mem)]

  [(ΨΓ⊢ifeee⇒T Ψ Γ Γ_lcl e_cnd e_thn e_els T_ret)
   ----------------------- "if expression"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (if e_cnd e_thn e_els) T_ret)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_fun T_fun)
   (as-fun Ψ T_fun (len (e_arg ...)) (-> ([x+☠_arg T_arg] ...) T_ret))
   (ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_arg T_arg) ...
   ----------------------- "function application"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (e_fun e_arg ...) T_ret)]

  [(ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_fun (type-op "cast"))
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl e_cls (classitself cid))
   (ΨΓΓ⊢e⇐T Ψ Γ Γ_lcl e_src (instancesof cid))
   ----------------------- "cast"
   (ΨΓ⊢e⇒T Ψ Γ Γ_lcl (e_fun e_cls e_src) (instancesof cid))]
  )
