#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "statics-basic-definitions.rkt")
(require "statics-utilities.rkt")
(require "statics-meaning-of-types.rkt")
(provide (all-defined-out))

(module+ test
  (check-judgment-holds*
   (¬⊢flat-class (base-Ψ) ((["x" (instancesof "int")])
                           (["x" () (instancesof "int")])))
   (¬⊢flat-class (base-Ψ) ((["x" (instancesof "int")] ["x" (instancesof "str")])
                           ()))
   (¬⊢flat-class (base-Ψ) (()
                           (["x" () (instancesof "str")] ["x" () (instancesof "int")])))
   (¬⊢flat-class (base-Ψ) ((["x" (instancesof "int")] ["x" dynamic])
                           ()))))
(define-judgment-form SP-statics
  #:mode (¬⊢flat-class I I)
  #:contract (¬⊢flat-class Ψ flat-class)

  ;; Is this flat-class ill-formed under the class environment K?

  [------------------"field-and-method"
   (¬⊢flat-class Ψ ((any_0 ... (string any_fieldspec ...) any_1 ...)
                    (any_1 ... (string any_methodspec ...) any_2 ...)))]

  [(where #t (≠ T_0 T_1))
   ------------------"field-twice"
   (¬⊢flat-class Ψ ((any_0 ...
                     (string_0 T_0)
                     any_1 ...
                     (string_0 T_1)
                     any_2 ...)
                    any_methods))]

  [(where #f (Ψ⊢T≲T Ψ (-> ([x+☠_c T_arg1] ...) T_ret1) (-> ([x+☠_p T_arg2] ...) T_ret2)))
   ;; don't worry about constructors
   (where #t (≠ string_0 "__init__"))
   ------------------"method-incompatible"
   (¬⊢flat-class Ψ
                 (any_fields
                  (any_0 ...
                   [string_0 ([x+☠_c T_arg1] ...) T_ret1]
                   any_1 ...
                   [string_0 ([x+☠_p T_arg2] ...) T_ret2]
                   any_2 ...)))])

(define-judgment-form SP-statics
  #:mode (⊢flat-class I I)
  #:contract (⊢flat-class Ψ flat-class)

  ;; Is this flat-class well-formed under the class environment K?

  [(where #f (¬⊢flat-class Ψ flat-class))
   -----------------
   (⊢flat-class Ψ flat-class)])

(define-judgment-form SP-statics
  #:mode (Γ-overrides I I)
  #:contract (Γ-overrides Ψ Γ)

  [;(where #f (Ψ⊢T≲T Ψ T_1 T_2))
   --------------------------
   (Γ-overrides
    Ψ
    (any_1 ... [x T_1] any_2 ... [x T_2] any_3 ...))])

(module+ test
  (check-judgment-holds*
   (⊢p ((class B (object))))
   (⊢p ((class B (object))
        (expr (B))))
   (⊢p ((class B (object) (field "x" int))
        (class C (B))
        (expr (C))))
   (⊢p ((def expectInt ((i int)) dynamic pass)
        (expr expectInt)))
   (⊢p ((import-from "__static__" ("cast"))
        (expr (cast bool #t))))
   (⊢p ((import-from "typing" ("Optional"))
        (define/assign x (subscript Optional int) 2)))
   (⊢p ((import-from "typing" ("Optional"))
        (define/assign x (subscript Optional int) None)))
   (⊢p ((if #t
            ((define/assign x int 2))
            ((define/assign x int 3)))
        (define/assign y int x)))
   (⊢p ((def f () None
          (if #t
              ((define/assign x int 2))
              ((define/assign x int 3)))
          (define/assign y int x))))))
(define-judgment-form SP-statics
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_1 (base-Ψ))
   (where Γ_1 (base-Γ))
   (where Γ_2 (collect-imports Γ_1 import-type ...))
   (where #f (¬⊢s* (s ...)))
   (where (Ψ_2 Γ_3) (collect-defs-and-clss Ψ_1 Γ_2 s ...))
   (where ([cid C] ...) Ψ_2)
   (Ψ⊢cid Ψ_2 cid) ...
   (ΨΓ⊢s*⇐T+☠ Ψ_2 Γ_3 (s ...) ☠)
   ------------------------
   (⊢p (import-type ... s ...))])

(module+ test
  (test-equal
   (term (collect-imports ()))
   (term ())))

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
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)))
              (term ((base-Ψ) (base-Γ))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (define/assign foo int 42)))
              (term ((base-Ψ)
                     (extend (base-Γ) [foo (instancesof "int")]))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (def foo ([arg str]) int (return 42))))
              (term ((base-Ψ)
                     (extend (base-Γ) [foo (-> ([arg (instancesof "str")]) (instancesof "int"))]))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (class C (object))))
              (term ((extend (base-Ψ) [0 (class C "object" () ())])
                     (extend (base-Γ) [C (classitself 0)]))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (class C (object) (field "x" int))))
              (term ((extend (base-Ψ) [0 (class C "object" (["x" (instancesof "int")]) ())])
                     (extend (base-Γ) [C (classitself 0)]))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (class B ())
                                           (class C (object))))
              (term ((extend (base-Ψ) [1 (class C "object" () ())] [0 (class B "object" () ())])
                     (extend (base-Γ) [C (classitself 1)] [B (classitself 0)])))))

(define-metafunction SP-statics
  collect-ext : s ... -> ([x any] ...)
  ;; What are the classes and variables defined at the top level?
  [(collect-ext) ()]
  [(collect-ext (define/assign x t e) s ...)
   (extend (collect-ext s ...) [x define/assign])]
  [(collect-ext (def x ([x_arg t_arg] ...) t_ret s_body ...) s ...)
   (extend (collect-ext s ...) [x def/class])]
  [(collect-ext (class x (t_par ...) class-member ...) s ...)
   (extend (collect-ext s ...) [x def/class])]
  [(collect-ext (if e (s_thn ...) (s_els ...)) s ...)
   (collect-ext s_thn ... s_els ... s ...)]
  [(collect-ext s_1 s_2 ...)
   (collect-ext s_2 ...)])

(define-judgment-form SP-statics
  #:mode (¬⊢s* I)
  #:contract (¬⊢s* (s ...))

  ;; Are there any conflicting expressions?

  [(where (any_1 ... [x def/class] any_2 ... [x any] any_3 ...)
          (collect-ext s ...))
   ---------------------- "def/class-L"
   (¬⊢s* (s ...))]

  [(where (any_1 ... [x any] any_2 ... [x def/class] any_3 ...)
          (collect-ext s ...))
   ---------------------- "def/class-R"
   (¬⊢s* (s ...))])

(define-metafunction SP-statics
  compute-parent : T ... -> cid+dynamic+☠
  [(compute-parent) "object"]
  [(compute-parent (instancesof cid)) cid]
  [(compute-parent T ...) dynamic])

(define-metafunction SP-statics
  collect-defs-and-clss : Ψ Γ s ... -> (Ψ Γ)
  ;; What are the classes and variables defined at the top level?
  
  [(collect-defs-and-clss Ψ Γ) (Ψ Γ)]

  ;; define variable
  [(collect-defs-and-clss Ψ Γ_1 (define/assign x t e) s ...)
   (collect-defs-and-clss Ψ Γ_2 s ...)
   (judgment-holds (evalo Ψ Γ_1 t T))
   (where Γ_2 (extend Γ_1 [x T]))]

  ;; define function
  [(collect-defs-and-clss Ψ Γ_1 (def x_fun ([x_arg t_arg] ...) t_ret s_body ...) s ...)
   (collect-defs-and-clss Ψ Γ_2 s ...)
   (judgment-holds (evalo* Ψ Γ_1 (t_arg ...) (T_arg ...)))
   (judgment-holds (evalo Ψ Γ_1 t_ret T_ret))
   (where T (-> ([x_arg T_arg] ...) T_ret))
   (where Γ_2 (extend Γ_1 [x_fun T]))]

  ;; define class
  [(collect-defs-and-clss Ψ_1 Γ_1 (class x (t_par ...) class-member ...) s ...)
   (collect-defs-and-clss Ψ_2 Γ_2 s ...)
   (judgment-holds (evalo* Ψ_1 Γ_1 (t_par ...) (T_par ...)))
   (where (((string_fld t_fld) ...)
           ((string_mth ([x_arg t_arg] ...) t_ret) ...))
          (collect-mems class-member ...))
   (judgment-holds (evalo* Ψ_1 Γ_1 (t_fld ...) (T_fld ...)))
   (judgment-holds (evalo** Ψ_1 Γ_1
                            ((t_arg ...) ...)
                            ((T_arg ...) ...)))
   (judgment-holds (evalo* Ψ_1 Γ_1
                           (t_ret ...)
                           (T_ret ...)))
   (where C (class x (compute-parent T_par ...)
              ((string_fld T_fld) ...)
              ((string_mth ([x_arg T_arg] ...) T_ret) ...)))
   (where (Ψ_2 cid) (Ψ-alloc Ψ_1 C))
   (where Γ_2 (extend Γ_1 [x (classitself cid)]))]

  [(collect-defs-and-clss Ψ_1 Γ_1 (def any ...) s ...)
   ,(error "should not come here" (term (Ψ_1 Γ_1 (def any ...))))]

  [(collect-defs-and-clss Ψ_1 Γ_1 (class any ...) s ...)
   ,(error "should not come here" (term (Ψ_1 Γ_1 (class any ...))))]

  ;; ignore the define/assign if the lhs is not a variable
  [(collect-defs-and-clss Ψ Γ (define/assign e t e) s ...)
   (collect-defs-and-clss Ψ Γ s ...)]

  ;; handle if branches, TODO: this might be a bit simplisitic
  [(collect-defs-and-clss Ψ Γ (if e (s_thn ...) (s_els ...)) s ...)
   (collect-defs-and-clss Ψ Γ s_thn ... s_els ... s ...)]
  
  [(collect-defs-and-clss Ψ Γ s_fst s_rst ...)
   (collect-defs-and-clss Ψ Γ s_rst ...)])

(define-metafunction SP-statics
  collect-local-defs-and-clss : Ψ Γ s ... -> Γ
  ;; What are the classes and variables defined at the top level?
  
  [(collect-local-defs-and-clss Ψ Γ) Γ]

  ;; define variable
  [(collect-local-defs-and-clss Ψ Γ_1 (define/assign x t e) s ...)
   (collect-local-defs-and-clss Ψ Γ_2 s ...)
   (judgment-holds (evalo Ψ Γ_1 t T))
   (where Γ_2 (extend Γ_1 [x T]))]

  ;; define function
  [(collect-local-defs-and-clss Ψ Γ_1 (def x_fun ([x_arg t_arg] ...) t_ret s_body ...) s ...)
   (collect-local-defs-and-clss Ψ Γ_2 s ...)
   (where T (-> ([x_arg T_arg] ...) T_ret))
   (judgment-holds (evalo Ψ Γ_1 (t_arg ...) (T_arg ...)))
   (judgment-holds (evalo Ψ Γ_1 t_ret T_ret))
   (where Γ_2 (extend Γ_1 [x T]))]

  ;; local define class doesn't go into Ψ, they are treated as dynamic
  [(collect-local-defs-and-clss Ψ Γ_1 (class x (t_parent ...) class-member ...) s ...)
   (collect-local-defs-and-clss Ψ Γ_2 s ...)
   (where Γ_2 (extend Γ_1 [x dynamic]))]

  ;; ignore the define/assign if the lhs is not a variable
  [(collect-local-defs-and-clss Ψ Γ (define/assign e t e) s ...)
   (collect-local-defs-and-clss Ψ Γ s ...)]
  
  ;; handle if branches, TODO: this might be a bit simplisitic
  [(collect-local-defs-and-clss Ψ Γ (if e (s_thn ...) (s_els ...)) s ...)
   (collect-local-defs-and-clss Ψ Γ s_thn ... s_els ... s ...)]

  [(collect-local-defs-and-clss Ψ Γ s_fst s_rst ...)
   (collect-local-defs-and-clss Ψ Γ s_rst ...)])

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


(define-metafunction SP-statics
  collect-mems : class-member ... -> (((string_field t_field) ...)
                                      ((string_method ([x t_arg] ...) t_ret) ...))
  ;; What are the fields and methods defined in this class?

  [(collect-mems) (()
                   ())]

  [(collect-mems (field string t) class-member ...)
   (((string t) any_field ...)
    (any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))]

  [(collect-mems
    (method string_method x_self ([x_arg t_arg] ...) t_ret s ...)
    class-member ...)
   ((any_field ...)
    ((string_method ([x_arg t_arg] ...) t_ret) any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))])

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
             (def f ([x int]) None pass)
             dynamic)
   (ΨΓ⊢s⇐T+☠ (base-Ψ)
             (base-Γ)
             (delete 2)
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
             dynamic)))

(define-judgment-form SP-statics
  #:mode (ΨΓ⊢s⇐T+☠ I I I I)
  #:contract (ΨΓ⊢s⇐T+☠ Ψ Γ s T+☠)

  ;; Is the statement s well-formed under the type and class environment Γ?

  [(evalo Ψ Γ t T)
   (ΨΓ⊢e⇐T Ψ Γ e_1 T)
   (ΨΓ⊢e⇐T Ψ Γ e_2 T)
   ------------------------ "define/assign"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (define/assign e_1 t e_2) _)]

  [(evalo Ψ Γ t_arg T_arg) ...
   (evalo Ψ Γ t_ret T_ret)
   (where Γ_ext (collect-local-defs-and-clss Ψ Γ s ...))
   (Ψ⊢Γ Ψ Γ_ext)
   (where ([x_loc T_loc] ...) Γ_ext)
   (where Γ_body (extend Γ  [x_loc T_loc] ... [x_arg T_arg] ...))
   (ΨΓ⊢s*⇐T+☠ Ψ Γ_body (s ...) T_ret)
   ------------------------ "def"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (def x_fun ([x_arg t_arg] ...) t_ret s ...) _)]


  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "delete"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (delete e) _)]


  [------------------------ "pass"
   (ΨΓ⊢s⇐T+☠ Ψ Γ pass _)]


  [(ΨΓ⊢class-member Ψ Γ class-member) ...
   ------------------------ "class"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (class x (t ...) class-member ...) _)]

  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "expr"
   (ΨΓ⊢s⇐T+☠ Ψ Γ (expr e) _)])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (base-Γ) () ☠)
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (base-Γ) () (instancesof "None"))
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (base-Γ) () dynamic)
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (base-Γ) () ("optional" "int"))
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (base-Γ) ((return 42) (return "foo")) (instancesof "int"))
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")])
              ((if (is x None)
                   ((return 42))
                   ())
               (return x))
              (instancesof "int"))
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")])
              ((if (is-not x None)
                   ()
                   ((return 42)))
               (return x))
              (instancesof "int"))
   (ΨΓ⊢s*⇐T+☠ (base-Ψ) (extend (base-Γ) [x ("optional" "int")] [y ("optional" "int")])
              ((if (bool-op and (is-not x None) (is-not y None))
                   ((return (bin-op + x y)))
                   ((return 42))))
              (instancesof "float"))))
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢s*⇐T+☠ I I I I)
  #:contract (ΨΓ⊢s*⇐T+☠ Ψ Γ (s ...) T+☠)

  [--------------------- "end of file"
   (ΨΓ⊢s*⇐T+☠ Ψ Γ () ☠)]

  [(ΨΓ⊢s*⇐T+☠ Ψ Γ ((return None)) T)
   --------------------- "no return is return None"
   (ΨΓ⊢s*⇐T+☠ Ψ Γ () T)]
  
  [(ΨΓ⊢e⇐T Ψ Γ e T)
   --------------------- "return"
   (ΨΓ⊢s*⇐T+☠ Ψ Γ ((return e) s ...) T)]

  [(ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ e (s_thn ... s ...) (s_els ... s ...) T+☠)
   --------------------- "if"
   (ΨΓ⊢s*⇐T+☠ Ψ Γ ((if e (s_thn ...) (s_els ...)) s ...) T+☠)]

  [(ΨΓ⊢s⇐T+☠ Ψ Γ s_1 T+☠)
   (ΨΓ⊢s*⇐T+☠ Ψ Γ (s_2 ...) T+☠)
   -----------------
   (ΨΓ⊢s*⇐T+☠ Ψ Γ (s_1 s_2 ...) T+☠)])


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢ifes*s*⇐T+☠ I I I I I I)
  #:contract (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ e (s ...) (s ...) T+☠)

  [(ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ (is x None) (s_els ...) (s_thn ...) T+☠)
   ------------------ "if is-not"
   (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ (is-not x None) (s_thn ...) (s_els ...) T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ x T)
   (ΨΓ⊢s*⇐T+☠ Ψ (extend Γ [x (instancesof "None")]) (s_thn ...) T+☠)
   (ΨΓ⊢s*⇐T+☠ Ψ (extend Γ [x (remove-None Ψ T)]) (s_els ...) T+☠)
   ------------------ "if is"
   (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ (is x None) (s_thn ...) (s_els ...) T+☠)]

  [(ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ e (s_thn ...) (s_els ...) T+☠)
   ------------------ "if and-base"
   (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ (bool-op and e) (s_thn ...) (s_els ...) T+☠)]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 (instancesof "bool"))
   (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ e_1 ((if (bool-op and e_2 e_3 ...) (s_thn ...) (s_els ...))) (s_els ...) T+☠)
   ------------------ "if and-step"
   (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ (bool-op and e_1 e_2 e_3 ...) (s_thn ...) (s_els ...) T+☠)]

  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   (ΨΓ⊢s*⇐T+☠ Ψ Γ (s_thn ...) T+☠)
   (ΨΓ⊢s*⇐T+☠ Ψ Γ (s_els ...) T+☠)
   ------------------ "usual if"
   (ΨΓ⊢ifes*s*⇐T+☠ Ψ Γ e (s_thn ...) (s_els ...) T+☠)])


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
  #:mode (ΨΓ⊢class-member I I I)
  #:contract (ΨΓ⊢class-member Ψ Γ class-member)

  ;; Is this class-member well-formed under the environment Γ?

  [-------------------------
   (ΨΓ⊢class-member Ψ Γ (field string t))]

  [(evalo Ψ Γ t_arg T_arg) ...
   (evalo Ψ Γ t_ret T_ret)
   (ΨΓ⊢s*⇐T+☠ Ψ (extend Γ [x_self dynamic] [x_arg T_arg] ...) (s ...) T_ret)
   ;; TODO: the type of self looks weird
   -------------------------
   (ΨΓ⊢class-member Ψ Γ (method string_method x_self ([x_arg t_arg] ...) t_ret s ...))])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) None (instancesof "None"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) #t (instancesof "int"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) 42 (instancesof "int"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) (dict-syntax) (instancesof "dict"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) (set-syntax) (instancesof "set"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) (subscript (dict-syntax ("foo" 1)) "foo") dynamic)
   (ΨΓ⊢e⇐T (base-Ψ) (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           (subscript CheckedDict (tuple-syntax str int))
           (classitself ("CheckedDict" "str" "int")))
   (ΨΓ⊢e⇐T (base-Ψ) (extend (base-Γ) [d (instancesof ("CheckedDict" "str" "int"))])
           (subscript d "foo")
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
   (as-subscriptable (base-Ψ) (instancesof ("CheckedDict" "str" "int")) (instancesof "str") (instancesof "int"))))
(define-judgment-form SP-statics
  #:mode (as-subscriptable I I O O)
  #:contract (as-subscriptable Ψ T T T)

  [(where T_getitem (lookup-member Ψ T "__getitem__"))
   (as-fun Ψ T_getitem 1 (-> ([any T_key]) T_val))
   ---
   (as-subscriptable Ψ T T_key T_val)])

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
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (subscript (dict-syntax ("foo" 1)) "foo") dynamic)
   (ΨΓ⊢e⇒T (base-Ψ) (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           (subscript CheckedDict (tuple-syntax str int))
           (classitself ("CheckedDict" "str" "int")))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (attribute (dict-syntax) "__getitem__") (-> ([☠ dynamic]) dynamic))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (unary-op - 2) (instancesof "float"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (bin-op + 2 3) (instancesof "float"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (in None (set-syntax "foo" 42)) (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (bool-op and #t #f) dynamic)
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

  [(ΨΓ⊢e⇒T Ψ Γ ((attribute e "__neg__")) T)
   ------------------------ "unary-op -"
   (ΨΓ⊢e⇒T Ψ Γ (unary-op - e) T)]

  [(ΨΓ⊢e⇒T Ψ Γ ((attribute e_1 "__add__") e_2) T)
   ------------------------ "binop +"
   (ΨΓ⊢e⇒T Ψ Γ (bin-op + e_1 e_2) T)]
  
  [(ΨΓ⊢e⇒T Ψ Γ e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ e_2 T_2)
   ------------------------ "is"
   (ΨΓ⊢e⇒T Ψ Γ (is e_1 e_2) (instancesof "bool"))]

  [(ΨΓ⊢e⇒T Ψ Γ e_1 T_1)
   (ΨΓ⊢e⇒T Ψ Γ e_2 T_2)
   ------------------------ "is-not"
   (ΨΓ⊢e⇒T Ψ Γ (is-not e_1 e_2) (instancesof "bool"))]

  [(ΨΓ⊢e⇒T Ψ Γ ((attribute e_2 "__contains__") e_1) T)
   ------------------------ "in"
   (ΨΓ⊢e⇒T Ψ Γ (in e_1 e_2) T)]

  [(ΨΓ⊢e⇒T Ψ Γ ((attribute e_1 "__and__") e_2) T)
   ------------------------ "and"
   (ΨΓ⊢e⇒T Ψ Γ (bool-op and e_1 e_2) T)]
  
  [(where (subscript x (tuple-syntax t ...)) e)
   (evalo Ψ Γ e (instancesof cid))
   ------------------------ "generic"
   (ΨΓ⊢e⇒T Ψ Γ e (classitself cid))]

  [(lookupo Γ x T)
   ------------------------ "variable"
   (ΨΓ⊢e⇒T Ψ Γ x T)]

  [(ΨΓ⊢e⇒T Ψ Γ e_map T_map)
   (as-subscriptable Ψ T_map T_key T_val)
   (ΨΓ⊢e⇐T Ψ Γ e_key T_key)
   ----------------------- "subscription"
   (ΨΓ⊢e⇒T Ψ Γ (subscript e_map e_key) T_val)]

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
