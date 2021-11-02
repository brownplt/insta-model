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
   (ΨΓ⊢s⇐T+☠ Ψ_2 Γ_4 s ☠)
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
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (base-Γ)
             (return 2)
             ☠))
  (check-judgment-holds*
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (extend (base-Γ) [x (instancesof "int")])
             (define/assign x int 42)
             dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (base-Γ)
             (def f ([x int]) None () pass)
             dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (base-Γ)
             pass
             dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (base-Γ)
             (class C ())
             dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (base-Γ)
             (expr None)
             dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (begin) ☠)
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (begin) (instancesof "None"))
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (begin) dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (begin) ("optional" "int"))
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (base-Γ) (begin (return 42) (return "foo")) (instancesof "int"))
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")])
             (begin
               (if (is x None)
                   (begin (return 42))
                   (begin))
               (return x))
             (instancesof "int"))
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")])
             (begin
               (if (is-not x None)
                   (begin)
                   (begin (return 42)))
               (return x))
             (instancesof "int"))
   (ΨΓ⊢s⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")] [y ("optional" "int")])
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
  #:mode (ΨΓ⊢s⇐T+☠ I I I I)
  #:contract (ΨΓ⊢s⇐T+☠ Ψ Γ s T+☠)
  ;; DO INDUCTION ON S
  ;; Is the statement s well-formed under the type and class environment Γ?

  [------------------------ "claim"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (claim x t) _)]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 T)
   (ΨΓ⊢e⇐T Ψ Γ e_2 T)
   ------------------------ "define/assign"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (define/assign e_1 t e_2) _)]

  [(evalo Ψ Γ_1 t_ret T_ret)
   (where d_2 (extend d_1 [x_arg t_arg] ...))
   (where d_3 (simplify-d d_2))
   (where #f (some-redeclaration? d_3))
   (where (([x_cls any] ...) ([x_var D_var] ...)) (split-d d_3))
   (evalD Ψ Γ_1 D_var T_var) ...
   (ΨΓ⊢s⇐T+☠ Ψ (extend Γ_1 [x_cls dynamic] ... [x_var T_var] ...) s T_ret)
   ------------------------ "def"
   (ΨΓ⊢s⇐T+☠ Ψ Γ_1 (def x_fun ([x_arg t_arg] ...) t_ret d_1 s) _)]
  
  [(ΨΓ⊢m Ψ Γ m) ...
   ------------------------ "class"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (class x (t ...) m ...) _)]

  ;; If statements are handled elsewhe

  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "delete"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (delete e) _)]

  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "expr"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (expr e) _)]
  
  [------------------------ "pass"
   (ΨΓ⊢s⇐T+☠ Ψ Γ pass _)]

  [(ΨΓ⊢e⇐T Ψ Γ e T)
   ------------------------ "return"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (return e) T)]
  
  [(ΨΓ⊢ifess⇐T+☠ Ψ Γ e s_thn s_els T+☠)
   --------------------------------------- "if"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (if e s_thn s_els) T+☠)]
  
  ;; begin-related

  [--------------------- "begin-end no expect"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin) ☠)]

  [(ΨΓ⊢s⇐T+☠ Ψ Γ (return None) T)
   --------------------- "begin-end some expect"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin) T)]
  
  [(ΨΓ⊢s⇐T+☠ Ψ Γ (return e) T+☠)
   --------------------- "begin-return"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin (return e) s ...) T+☠)]

  [(ΨΓ⊢s⇐T+☠ Ψ Γ (begin s_1 ... s_2 ...) T+☠)
   --------------------- "begin-begin"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin (begin s_1 ...) s_2 ...) T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ e T_1)
   (evalo Ψ Γ t T_2)
   (Ψ⊢T≲T Ψ T_1 T_2)
   (where T (intersection Ψ T_1 T_2))
   (ΨΓ⊢s⇐T+☠ Ψ (extend Γ [x T]) (begin s ...) T+☠)
   --------------------- "begin-define/assign"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin (define/assign x t e) s ...) T+☠)]

  [(ΨΓ⊢s⇐T+☠ Ψ Γ (if e (begin s_thn s ...) (begin s_els s ...)) T+☠)
   --------------------- "begin-if"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin (if e s_thn s_els) s ...) T+☠)]

  [(ΨΓ⊢s⇐T+☠ Ψ Γ s_1 T+☠)
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin s_2 ...) T+☠)
   ----------------- "begin-otherwise"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (begin s_1 s_2 ...) T+☠)]
  )


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢ifess⇐T+☠ I I I I I I)
  #:contract (ΨΓ⊢ifess⇐T+☠ Ψ Γ e s s T+☠)

  [(ΨΓ⊢ifess⇐T+☠ Ψ Γ (is x None) s_els s_thn T+☠)
   ------------------ "if is-not"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ (is-not x None) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ x T)
   (ΨΓ⊢s⇐T+☠ Ψ (extend Γ [x (instancesof "None")]) s_thn T+☠)
   (ΨΓ⊢s⇐T+☠ Ψ (extend Γ [x (remove-None T)]) s_els T+☠)
   ------------------ "if is"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ (is x None) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 (instancesof "bool"))
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ e_1 (if e_2 s_thn s_els) s_els T+☠)
   ------------------ "if and"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ (bool-op and e_1 e_2) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 (instancesof "bool"))
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ e_1 s_thn (if e_2 s_thn s_els) T+☠)
   ------------------ "if or"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ (bool-op or e_1 e_2) s_thn s_els T+☠)]

  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   (ΨΓ⊢s⇐T+☠ Ψ Γ s_thn T+☠)
   (ΨΓ⊢s⇐T+☠ Ψ Γ s_els T+☠)
   ------------------ "usual if"
   (ΨΓ⊢ifess⇐T+☠ Ψ Γ e s_thn s_els T+☠)])

;; TODO merge the two if judgments
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢ifeee⇒T I I I I I O)
  #:contract (ΨΓ⊢ifeee⇒T Ψ Γ e e e T)

  [(ΨΓ⊢ifeee⇒T Ψ Γ (is x None) e_els e_thn T)
   ------------------ "if is-not"
   (ΨΓ⊢ifeee⇒T Ψ Γ (is-not x None) e_thn e_els T)]

  [(ΨΓ⊢e⇒T Ψ Γ x ("optional" cid))
   (ΨΓ⊢e⇒T Ψ (extend Γ [x (instancesof "None")]) e_thn T_thn)
   (ΨΓ⊢e⇒T Ψ (extend Γ [x (instancesof cid)]) e_els T_els)
   ------------------ "if is"
   (ΨΓ⊢ifeee⇒T Ψ Γ (is x None) e_thn e_els (union Ψ T_thn T_els))]

  [(ΨΓ⊢e⇐T Ψ Γ e_cnd dynamic)
   (ΨΓ⊢e⇒T Ψ Γ e_thn T_thn)
   (ΨΓ⊢e⇒T Ψ Γ e_els T_els)
   ------------------ "usual if"
   (ΨΓ⊢ifeee⇒T Ψ Γ e_cnd e_thn e_els (union Ψ T_thn T_els))])


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢m I I I)
  #:contract (ΨΓ⊢m Ψ Γ m)

  ;; Is this m well-formed under the environment Γ?

  [-------------------------
   (ΨΓ⊢m Ψ Γ (field string t))]

  ;; TODO: the type of x_slf doesn't look right
  [(ΨΓ⊢s⇐T+☠ Ψ Γ (def whatever ([x_slf dynamic] [x_arg t_arg] ...) t_ret d s) ☠)
   -------------------------
   (ΨΓ⊢m Ψ Γ (method string_method x_slf ([x_arg t_arg] ...) t_ret d s))])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) None (instancesof "None"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) #t (instancesof "int"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) 42 (instancesof "int"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) (dict-syntax) (instancesof "dict"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) (set-syntax) (instancesof "set"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) ((attribute (dict-syntax ("foo" 1)) "__getitem__") "foo") dynamic)
   (ΨΓ⊢e⇐T (base-Ψ) (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           ((attribute CheckedDict "__getitem__") (tuple-syntax str int))
           (classitself ("CheckedDict" "str" "int")))
   (ΨΓ⊢e⇐T (base-Ψ) (extend (base-Γ) [d (instancesof ("CheckedDict" "str" "int"))])
           ((attribute d "__getitem__") "foo")
           (instancesof "int"))))


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢e⇐T I I I I)
  #:contract (ΨΓ⊢e⇐T Ψ Γ e T)
  ;; Is expression e well-formed under Γ and usable as a t?

  [(ΨΓ⊢e⇒T Ψ Γ e T_1)
   (Ψ⊢T≲T Ψ T_1 T_2)
   ------------------------ "switch"
   (ΨΓ⊢e⇐T Ψ Γ e T_2)]
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
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) int (classitself "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) None (instancesof "None"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) #t (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) 42 (instancesof "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (set-syntax) (instancesof "set"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (set-syntax "foo" 42) (instancesof "set"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (dict-syntax) (instancesof "dict"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (dict-syntax [42 "foo"] ["bar" 120]) (instancesof "dict"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) ((attribute (dict-syntax ("foo" 1)) "__getitem__") "foo") dynamic)
   (ΨΓ⊢e⇒T (base-Ψ) (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           ((attribute CheckedDict "__getitem__") (tuple-syntax str int))
           (classitself ("CheckedDict" "str" "int")))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (attribute (dict-syntax) "__getitem__") (-> ([☠ dynamic]) dynamic))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (bool-op and #t #f) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (bool-op or #t #f) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (int "123") (instancesof "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (is "foo" None) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (is-not 42 "x") (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ)
           (extend (base-Γ) [f (-> ([x (instancesof "int")]) (instancesof "str"))])
           (f 42)
           (instancesof "str"))))
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢e⇒T I I I O)
  #:contract (ΨΓ⊢e⇒T Ψ Γ e T)
  ;; Is expression e well-formed under type environment Γ and usable as a T?

  [----------------------- "None"
   (ΨΓ⊢e⇒T Ψ Γ None (instancesof "None"))]

  ;; TODO remove this
  [(ΨΓ⊢e⇒T Ψ Γ e T)
   (show (the-type-of e is T at any ...))
   ----------------------- "reveal-type"
   (ΨΓ⊢e⇒T Ψ Γ (reveal-type any ... e) T)]

  [----------------------- "integer"
   (ΨΓ⊢e⇒T Ψ Γ integer (instancesof "int"))]

  [----------------------- "boolean"
   (ΨΓ⊢e⇒T Ψ Γ boolean (instancesof "bool"))]

  [----------------------- "string"
   (ΨΓ⊢e⇒T Ψ Γ string (instancesof "str"))]

  [(ΨΓ⊢e⇐T Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐T Ψ Γ e_value dynamic) ...
   ----------------------- "dict"
   (ΨΓ⊢e⇒T Ψ Γ (dict-syntax [e_key e_value] ...) (instancesof "dict"))]

  [(ΨΓ⊢e⇐T Ψ Γ e_elt dynamic) ...
   ----------------------- "set"
   (ΨΓ⊢e⇒T Ψ Γ (set-syntax e_elt ...) (instancesof "set"))]
  
  [(ΨΓ⊢e⇒T Ψ Γ e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ e_2 T_2)
   ------------------------ "is"
   (ΨΓ⊢e⇒T Ψ Γ (is e_1 e_2) (instancesof "bool"))]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ e_2 T_2)
   ------------------------ "is-not"
   (ΨΓ⊢e⇒T Ψ Γ (is-not e_1 e_2) (instancesof "bool"))]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ e_2 T_2)
   ------------------------ "bool-op"
   (ΨΓ⊢e⇒T Ψ Γ (bool-op ob e_1 e_2) (union Ψ T_1 T_2))]
  
  [(where ((attribute x "__getitem__") (tuple-syntax t ...)) e)
   (evalo Ψ Γ e (instancesof cid))
   ------------------------ "generic"
   (ΨΓ⊢e⇒T Ψ Γ e (classitself cid))]

  [(lookupo Γ x T)
   ------------------------ "variable"
   (ΨΓ⊢e⇒T Ψ Γ x T)]

  [(ΨΓ⊢e⇒T Ψ Γ e_ins T_ins)
   (where T_mem (lookup-member Ψ T_ins string_mem))
   ----------------------- "attribute / access member"
   (ΨΓ⊢e⇒T Ψ Γ (attribute e_ins string_mem) T_mem)]

  [(ΨΓ⊢ifeee⇒T Ψ Γ e_cnd e_thn e_els T_ret)
   ----------------------- "if expression"
   (ΨΓ⊢e⇒T Ψ Γ (if e_cnd e_thn e_els) T_ret)]

  [(ΨΓ⊢e⇒T Ψ Γ e_fun T_fun)
   (as-fun Ψ T_fun (len (e_arg ...)) (-> ([x+☠_arg T_arg] ...) T_ret))
   (ΨΓ⊢e⇐T Ψ Γ e_arg T_arg) ...
   ----------------------- "function application"
   (ΨΓ⊢e⇒T Ψ Γ (e_fun e_arg ...) T_ret)]

  [(ΨΓ⊢e⇒T Ψ Γ e_fun (type-op "cast"))
   (ΨΓ⊢e⇒T Ψ Γ e_cls (classitself cid))
   (ΨΓ⊢e⇐T Ψ Γ e_src (instancesof cid)) 
   ----------------------- "cast"
   (ΨΓ⊢e⇒T Ψ Γ (e_fun e_cls e_src) (instancesof cid))]
  )
