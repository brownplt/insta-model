#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
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
        (expr (cast bool #t))))))
(define-judgment-form SP-statics
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_1 (base-Ψ))
   (where Γ_1 (base-Γ))
   (where Γ_2 (collect-imports Γ_1 import-type ...))
   (where (Ψ_2 Γ_3) (collect-defs-and-clss Ψ_1 Γ_2 s ...))
   (where ([cid C] ...) Ψ_2)
   (Ψ⊢cid Ψ_2 cid) ...
   (ΨΓ⊢s⇐T Ψ_2 Γ_3 s dynamic) ...
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
  (test-equal (term (collect-one-import (base-Γ) "typing" "Any"))
              (term (extend (base-Γ) [Any dynamic])))
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
  ;; There are no more things from __static__ doesn't exist
  [(collect-one-import Γ "__static__" string)
   #f]
  ;; typing provides Any
  [(collect-one-import Γ "typing" "Any")
   (extend Γ [Any dynamic])]
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
              (term ((extend (base-Ψ) [0 (class C ("object") () ())])
                     (extend (base-Γ) [C (classitself 0)]))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (class C (object) (field "x" int))))
              (term ((extend (base-Ψ) [0 (class C ("object") (["x" (instancesof "int")]) ())])
                     (extend (base-Γ) [C (classitself 0)]))))
  (test-equal (term (collect-defs-and-clss (base-Ψ) (base-Γ)
                                           (class B (object))
                                           (class C (object))))
              (term ((extend (base-Ψ) [1 (class C ("object") () ())] [0 (class B ("object") () ())])
                     (extend (base-Γ) [C (classitself 1)] [B (classitself 0)])))))
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
  [(collect-defs-and-clss Ψ_1 Γ_1 (class x (t_parent ...) class-member ...) s ...)
   (collect-defs-and-clss Ψ_2 Γ_2 s ...)
   (judgment-holds (evalo* Ψ_1 Γ_1 (t_parent ...) ((instancesof cid_parent) ...)))
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
   (where C (class x (cid_parent ...)
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
  (check-judgment-holds*
   (ΨΓ⊢s⇐T (base-Ψ)
           (base-Γ)
           (define/assign x int 42)
           dynamic)))

(define-judgment-form SP-statics
  #:mode (ΨΓ⊢s⇐T I I I I)
  #:contract (ΨΓ⊢s⇐T Ψ Γ s T)

  ;; Is the statement s well-formed under the type and class environment Γ?

  [(ΨΓ⊢e⇐T Ψ Γ e T_ret)
   ------------------------ "return"
   (ΨΓ⊢s⇐T Ψ Γ (return e) T_ret)]

  [(evalo Ψ Γ t T)
   (ΨΓ⊢e⇐T Ψ Γ e T)
   ------------------------ "define/assign-check"
   (ΨΓ⊢s⇐T Ψ Γ (define/assign x t e) _)]

  [(where #f ,(redex-match? SP-statics x (term e_1)))
   (ΨΓ⊢e⇒T Ψ Γ e_1 T)
   (ΨΓ⊢e⇐T Ψ Γ e_2 T)
   ------------------------ "define/assign-field-update"
   (ΨΓ⊢s⇐T Ψ Γ (define/assign e_1 dynamic e_2) _)]

  [(evalo Ψ Γ t_arg T_arg) ...
   (evalo Ψ Γ t_ret T_ret)
   (where Γ_ext (collect-local-defs-and-clss Ψ Γ s ...))
   (Ψ⊢Γ Ψ Γ_ext)
   (where ([x_loc T_loc] ...) Γ_ext)
   (where Γ_body (extend Γ  [x_loc T_loc] ... [x_arg T_arg] ...))
   (ΨΓ⊢s⇐T Ψ Γ_body s T_ret) ...
   ------------------------ "def"
   (ΨΓ⊢s⇐T Ψ Γ (def x_fun ([x_arg t_arg] ...) t_ret s ...) _)]


  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "delete"
   (ΨΓ⊢s⇐T Ψ Γ (delete e) _)]


  [------------------------ "pass"
   (ΨΓ⊢s⇐T Ψ Γ pass _)]


  [(ΨΓ⊢class-member Ψ Γ class-member) ...
   ------------------------ "class"
   (ΨΓ⊢s⇐T Ψ Γ (class any_child (x_parent ...) class-member ...) _)]


  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "expr"
   (ΨΓ⊢s⇐T Ψ Γ (expr e) _)])


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢class-member I I I)
  #:contract (ΨΓ⊢class-member Ψ Γ class-member)

  ;; Is this class-member well-formed under the environment Γ?

  [-------------------------
   (ΨΓ⊢class-member Ψ Γ (field string t))]

  [(evalo Ψ Γ t_ret T_ret)
   (ΨΓ⊢s⇐T Ψ Γ s T_ret) ...
   -------------------------
   (ΨΓ⊢class-member Ψ Γ (method string_method x_self ((x_arg t_arg) ...) t_ret s ...))])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) None (instancesof "None"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) #t (instancesof "int"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) 42 (instancesof "int"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓ⊢e⇐T (base-Ψ) (base-Γ) (dict-syntax) (instancesof "dict"))
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
  ;; Is e well-formed under Γ and usable as a t?

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
   (as-fun (extend (base-Ψ) [0 (class MyClass ("object") () ())])
           (classitself 0) 0 (-> () (instancesof 0)))
   (as-fun (extend (base-Ψ) [0 (class MyClass ("str") () ())])
           (classitself 0) 1 (-> ([☠ dynamic]) (instancesof 0)))
   (as-fun (extend (base-Ψ) [0 (class MyClass ("object") () (["__init__" ([x (instancesof "int")]) dynamic]))])
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

  [---
   (as-subscriptable Ψ dynamic dynamic dynamic)]

  [(where T_getitem (lookup-member Ψ cid "__getitem__"))
   (as-fun Ψ T_getitem 1 (-> ([any T_key]) T_val))
   ---
   (as-subscriptable Ψ (instancesof cid) T_key T_val)])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) None (instancesof "None"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) #t (instancesof "bool"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) 42 (instancesof "int"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) "foo" (instancesof "str"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (dict-syntax) (instancesof "dict"))
   (ΨΓ⊢e⇒T (base-Ψ) (base-Γ) (subscript (dict-syntax ("foo" 1)) "foo") dynamic)
   (ΨΓ⊢e⇒T (base-Ψ) (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
           (subscript CheckedDict (tuple-syntax str int))
           (classitself ("CheckedDict" "str" "int")))
   (ΨΓ⊢e⇒T (base-Ψ) (extend (base-Γ) [d (instancesof ("CheckedDict" "str" "int"))])
           (subscript d "foo")
           (instancesof "int"))))
(define-judgment-form SP-statics
  #:mode (ΨΓ⊢e⇒T I I I O)
  #:contract (ΨΓ⊢e⇒T Ψ Γ e T)
  ;; Is e well-formed under Γ and usable as a T?

  [(where (subscript x (tuple-syntax t ...)) e)
   (evalo Ψ Γ e (instancesof cid))
   ------------------------ "generic"
   (ΨΓ⊢e⇒T Ψ Γ e (classitself cid))]

  [(lookupo Γ x T)
   ------------------------ "variable"
   (ΨΓ⊢e⇒T Ψ Γ x T)]

  [----------------------- "None"
   (ΨΓ⊢e⇒T Ψ Γ None (instancesof "None"))]

  [----------------------- "integer"
   (ΨΓ⊢e⇒T Ψ Γ integer (instancesof "int"))]

  [----------------------- "boolean"
   (ΨΓ⊢e⇒T Ψ Γ boolean (instancesof "bool"))]

  [----------------------- "string"
   (ΨΓ⊢e⇒T Ψ Γ string (instancesof "str"))]

  [(ΨΓ⊢e⇐T Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐T Ψ Γ e_value dynamic) ...
   ----------------------- "PyDict"
   (ΨΓ⊢e⇒T Ψ Γ (dict-syntax [e_key e_value] ...) (instancesof "dict"))]
  
  [(ΨΓ⊢e⇒T Ψ Γ e_map T_map)
   (as-subscriptable Ψ T_map T_key T_val)
   (ΨΓ⊢e⇐T Ψ Γ e_key T_key)
   ----------------------- "subscription"
   (ΨΓ⊢e⇒T Ψ Γ (subscript e_map e_key) T_val)]

  [(ΨΓ⊢e⇒T Ψ Γ e_ins (instancesof cid))
   (where T_mem (lookup-member Ψ cid string_mem))
   ----------------------- "attribute / access member"
   (ΨΓ⊢e⇒T Ψ Γ (attribute e_ins string_mem) T_mem)]

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

; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))


; (lookup Γ x) retrieves x's type from Γ
(module+ test
  (test-equal (term (lookup ((x int) (x None) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x None) (y int)) y)) (term int)))

