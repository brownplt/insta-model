#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))

(define-extended-language SP-statics SP
  ;; a global environment that maps class names to their definitions
  (Ψ ((any T) ...))
  ;; a local environment that maps variables to their types
  (Γ ((x T) ...))
  ;; syntactic types
  (t .... (quote T))
  ;; semantic types / type values
  (T dynamic
     (the-object-class) ;; the object class
     (prim-generic string)
     (generic string T ...)
     (-> ([x+☠ t] ...) t)
     (class any (x_parent ...)
       ((string_field t) ...)
       ((string_method ([x+☠ t] ...) t) ...))
     ;; classes themselves, useful in instance construction
     (class T))
  ;; a handy concept that is useful when I want to check well-formness of classes
  (flat-class (((string_field t_field) ...)
               ((string_method ([x+☠ t] ...) t) ...)))
  ;; optional flat-class
  (flat-class+☠ flat-class ☠)
  ;; optional variable
  (x+☠ ☠ x)
  ;; primtive class names
  (prim-class-name
   "dynamic"
   "object"
   "float"
   "int"
   "bool"
   "str"
   "dict"
   "None-class")
  )

(define-metafunction SP-statics
  prim-class : prim-class-name -> T
  [(prim-class prim-class-name)
   (lookup (base-Ψ) ,(string->symbol (term prim-class-name)))])

(define-metafunction SP-statics
  base-Ψ : -> Ψ
  [(base-Ψ) ((object (the-object-class))
             (type
              (class "type" (object)
                ()
                ()))
             (float
              (class "float" (object)
                ()
                (("__init__" ([☠ dynamic]) None)
                 ("__add__" ([☠ float] [☠ float]) float))))
             (int (class "int" (float) () ()))
             (bool (class "bool" (int) () ()))
             (str
              (class "str" (object)
                ()
                (("__init__" ([☠ dynamic]) None))))
             (dict
              (class "dict" (object)
                ()
                (("__init__" ([☠ dynamic]) None)
                 ("__getitem__" ([☠ dynamic]) dynamic))))
             (Callable (prim-generic "Callable"))
             (None-class
              (class "None" (object)
                ()
                (("__init__" ([☠ dynamic]) None)))))])

(define-metafunction SP-statics
  base-Γ : -> Γ
  [(base-Γ)
   ([x (class T)] ...)
   (where ([x T] ...) (base-Ψ))])

(define-judgment-form SP-statics
  #:mode (lookupo I I O)
  #:contract (lookupo ((any any) ...) any any)
  [(where #f (member any_key2 (any_key1 ...)))
   ---------------------- "Found"
   (lookupo ((any_key1 any_val1) ...
             (any_key2 any_val2)
             (any_key3 any_val3) ... )
            any_key2
            any_val2)])

(define-metafunction SP-statics
  lookup : ((any any) ...) any -> any
  [(lookup any_env any_key)
   any_val
   (judgment-holds (lookupo any_env any_key any_val))]
  [(lookup any_env any_key)
   ,(error 'lookup "can't find ~e in ~e" (term any_key) (term any_env))])

(define-judgment-form SP-statics
  #:mode (evalo I I O)
  #:contract (evalo Ψ t T)

  ;; Compute type values from type expressions

  [----
   (evalo Ψ dynamic dynamic)]

  [------------------------- "quotation"
   (evalo Ψ (quote T) T)]

  [------------------------- "None"
   (evalo Ψ None (prim-class "None-class"))]

  [(lookupo Ψ x (prim-generic "Callable"))
   ------------------------- "Callable"
   (evalo Ψ
          (subscript x (tuple-syntax t_input ... t_output))
          (-> ([☠ t_input] ...) t_output))]

  [(lookupo Ψ x (prim-generic "CheckedDict"))
   (evalo Ψ t_1 T_1)
   (evalo Ψ t_2 T_2)
   ------------------------- "CheckedDict"
   (evalo Ψ
          (subscript x (tuple-syntax t_1 t_2))
          (generic "CheckedDict" T_1 T_2))]

  [(lookupo Ψ x T)
   ------------------------- "Lookup"  ;; TODO: this doesn't look right to me. We should use Γ
   (evalo Ψ x T)])

(define-judgment-form SP-statics
  #:mode (Ψ⊢T≲T I I I)
  #:contract (Ψ⊢T≲T Ψ T T)
  ;; Is it sensible to use a value of class C_0 as as a value of
  ;; class C_1?
  ;; (a.k.a. consistent subtyping)

  [------------------------ "dynamic-L"
   (Ψ⊢T≲T Ψ dynamic T)]

  [------------------------ "dynamic-R"
   (Ψ⊢T≲T Ψ T dynamic)]

  [(side-condition
    (= (len (t_0i ...))
       (len (t_1i ...))))
   (evalo Ψ t_0i T_0i) ... (evalo Ψ t_0o T_0o)
   (evalo Ψ t_1i T_1i) ... (evalo Ψ t_1o T_1o)
   (Ψ⊢T≲T Ψ T_1i T_0i) ... (Ψ⊢T≲T Ψ T_0o T_1o)
   ------------------------ "tag-callable"
   (Ψ⊢T≲T Ψ (-> ([x+☠ t_0i] ...) t_0o) (-> ([x+☠ t_1i] ...) t_1o))]

  [------------------------ "C≲object"
   (Ψ⊢T≲T Ψ T (the-object-class))]

  [------------------------ "refl"
   (Ψ⊢T≲T Ψ T T)]

  [(where (class any_self-1 (x_parent-1)
            any_field-spec-1
            any_method-spec-1)
          T_1)
   (where (class any_self-2 (x_parent-2)
            any_field-spec-2
            any_method-spec-2)
          T_2)
   (where #t (≠ any_self-1 any_self-2))
   (lookupo Ψ x_parent-1 T_super)
   (Ψ⊢T≲T Ψ T_super T_2)
   ------------------------ "super"
   (Ψ⊢T≲T Ψ T_1 T_2)]
  )

(define-metafunction SP-statics
  member : any (any ...) -> boolean
  [(member any_0 (any_1 ... any_0 any_2 ...)) #t]
  [(member any_0 (any_1 ...)) #f])

(define-metafunction SP-statics
  not : boolean -> boolean
  [(not #t) #f]
  [(not #f) #t])

(define-metafunction SP-statics
  len : (any ...) -> number
  [(len (any ...)) ,(length (term (any ...)))])

(define-metafunction SP-statics
  = : any any -> boolean
  [(= any any) #t]
  [(= any_0 any_1) #f])

(define-metafunction SP-statics
  ≠ : any any -> boolean
  [(≠ any_0 any_1) (not (= any_0 any_1))])

(define-metafunction SP-statics
  extend : ((any any) ...) (any any) ... -> ((any any) ...)
  [(extend ((any_knownKey any_knownVal) ...) (any_newKey any_newVal) ...)
   ((any_newKey any_newVal) ... (any_knownKey any_knownVal) ...)])

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

(define-judgment-form SP-statics
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_0 (base-Ψ))
   (where Ψ_1 (collect-imports Ψ_0 import-type ...))
   (where Ψ_2 (collect-clss Ψ_1 s ...))
   (where Ψ Ψ_2)
   (where ([any T_cls] ...) Ψ)
   (Ψ⊢T Ψ T_cls) ...
   (where ([x_typ T_typ] ...) (collect-defs Ψ s ...))
   (where Γ (extend (base-Γ) [x_typ T_typ] ...))
   (Ψ⊢Γ Ψ Γ)
   (where #f (Γ-overrides Ψ Γ))
   #;(where #f ,(and (writeln (term Ψ))
                   (writeln (term Γ))
                   #f))
   (ΨΓ⊢s⇐T Ψ Γ s dynamic) ...
   ------------------------
   (⊢p (import-type ... s ...))])

(module+ test
  (test-equal
   (term (collect-imports ((object (the-object-class)))))
   (term ((object (the-object-class))))))

(define-metafunction SP-statics
  collect-one-import : Ψ string_mod string_var -> any
  [(collect-one-import Ψ "__static__" "PyDict")
   (extend Ψ [PyDict (prim-class "dict")])]
  [(collect-one-import Ψ "__static__" "CheckedDict")
   (extend Ψ [CheckedDict (prim-generic "CheckedDict")])]
  ;; cast doesn't extend Ψ
  [(collect-one-import Ψ "__static__" "cast")
   Ψ]
  ;; There are no more things from __static__ doesn't exist (TODO)
  [(collect-one-import Ψ "__static__" string)
   #f]
  ;; typing provides Any
  [(collect-one-import Ψ "typing" "Any")
   (extend Ψ [Any dynamic])]
  ;; everything else is dynamic
  [(collect-one-import Ψ string_mod string_var)
   (extend Ψ [,(string->symbol (term string_var)) dynamic])])

(define-metafunction SP-statics
  collect-imports : Ψ import-type ... -> any
  [(collect-imports Ψ) Ψ]
  [(collect-imports Ψ (import-from string ()) import-type ...)
   (collect-imports Ψ import-type ...)]
  [(collect-imports
    Ψ_1
    (import-from string_mod (string_var1 string_var2 ...))
    import-type ...)
   (collect-imports
    Ψ_2
    (import-from string_mod (string_var2 ...))
    import-type ...)
   (where Ψ_2 (collect-one-import Ψ_1 string_mod string_var1))]
  [(collect-imports any ...) #f])

(define-metafunction SP-statics
  collect-clss : Ψ s ... -> Ψ
  ;; What are the defined classes?
  [(collect-clss Ψ_0) Ψ_0]
  
  ;; single inheritance
  [(collect-clss Ψ_0 (class x_child (x_parent) class-member ...) s ...)
   Ψ_1
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where T_child (class x_child (x_parent) any_fields any_methods))
   (where Ψ_1 (collect-clss (extend Ψ_0 [x_child T_child]) s ...))]

  ;; multi inheritance
  [(collect-clss Ψ_0 (class x_child (x_parent1 x_parent2 ...) class-member ...) s ...)
   Ψ_1
   (where x_parent dynamic)
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where T_child (class x_child (x_parent) any_fields any_methods))
   (where Ψ_1 (collect-clss (extend Ψ_0 (x_child T_child)) s ...))]
  
  [(collect-clss Ψ_0 s_1 s_2 ...)
   (collect-clss Ψ_0 s_2 ...)])

(define-metafunction SP-statics
  collect-defs : Ψ s ... -> Γ
  ;; What are the classes and variables defined at the top level?
  ;; TODO: non-top-level collect-defs need to be different
  
  [(collect-defs Ψ) ()]

  [(collect-defs Ψ (define/assign x t e) s ...)
   (extend (collect-defs Ψ s ...)
           (x T))
   (judgment-holds (evalo Ψ t T))]

  [(collect-defs Ψ (def x_fun ([x_arg t_arg] ...) t_ret s_body ...) s ...)
   (extend (collect-defs Ψ s ...)
           (x_fun (-> ([x_arg t_arg] ...) t_ret)))]

  [(collect-defs Ψ (class x_cls (x_parent ...) class-member ...) s ...)
   (extend (collect-defs Ψ s ...)
           (x_cls (class T_cls)))
   (judgment-holds (evalo Ψ x_cls T_cls))]

  ;; ignore the define/assign if the lhs is not a variable
  [(collect-defs Ψ (define/assign e t e) s ...)
   (collect-defs Ψ s ...)]

  [(collect-defs Ψ s_fst s_rst ...)
   (collect-defs Ψ s_rst ...)])

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

(define-metafunction SP-statics
  flatten-class : Ψ T -> flat-class
  [(flatten-class Ψ T)
   flat-class
   (judgment-holds (flatten-classo Ψ T flat-class))]
  [(flatten-class Ψ T)
   ,(error "can't flatten ~a" (term T))])

(define-judgment-form SP-statics
  #:mode (flatten-classo I I O)
  #:contract (flatten-classo Ψ T flat-class)

  [--------------------- "checkeddict"
   (flatten-classo
    Ψ
    (generic "CheckedDict" T_key T_val)
    (()
     (("__init__" ([☠ (quote (prim-class "dict"))]) None)
      ("__getitem__" ([☠ (quote T_key)]) (quote T_val)))))]

  [--------------------- "object"
   (flatten-classo
    Ψ
    (the-object-class)
    (()
     (("__init__" () None))))]

  [(flatten-classo
    Ψ
    (lookup Ψ x_parent)
    ((any_field ...)
     (any_method ...)))
   ----------------------- "extended class"
   (flatten-classo
    Ψ
    (class any_self (x_parent)
      (any_new-field ...)
      (any_new-method ...))
    ((any_new-field ... any_field ...)
     (any_new-method ... any_method ...)))])

(define-judgment-form SP-statics
  #:mode (Ψ⊢T I I)
  #:contract (Ψ⊢T Ψ T)
  ;; Is this class well-formed (not overriding member signatures incorrectly)?

  [--------------------------------------
   (Ψ⊢T Ψ dynamic)]

  [--------------------------------------
   (Ψ⊢T Ψ (prim-generic string))]

  ;; no incompatible override
  [(flatten-classo Ψ T flat-class)
   (⊢flat-class Ψ flat-class)
   --------------------------------------
   (Ψ⊢T Ψ T)])


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
           ((x (lookup (base-Ψ) int)))
           (define/assign x int 42)
           dynamic)))

(define-judgment-form SP-statics
  #:mode (ΨΓ⊢s⇐T I I I I)
  #:contract (ΨΓ⊢s⇐T Ψ Γ s T)

  ;; Is the statement s well-formed under the type and class environment Γ?

  [(ΨΓ⊢e⇐T Ψ Γ e T_ret)
   ------------------------ "return"
   (ΨΓ⊢s⇐T Ψ Γ (return e) T_ret)]

  [(evalo Ψ t T)
   (ΨΓ⊢e⇐T Ψ Γ e T)
   ------------------------ "define/assign-check"
   (ΨΓ⊢s⇐T Ψ Γ (define/assign x t e) _)]

  [(where #f ,(redex-match? SP-statics x (term e_1)))
   (ΨΓ⊢e⇒T Ψ Γ e_1 T)
   (ΨΓ⊢e⇐T Ψ Γ e_2 T)
   ------------------------ "define/assign-field-update"
   (ΨΓ⊢s⇐T Ψ Γ (define/assign e_1 dynamic e_2) _)]

  [(evalo Ψ t_arg T_arg) ...
   (evalo Ψ t_ret T_ret)
   (where Γ_ext (collect-defs Ψ s ...))
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
  #:mode (¬⊢flat-class I I)
  #:contract (¬⊢flat-class Ψ flat-class)

  ;; Is this flat-class ill-formed under the class environment K?

  [------------------"field-and-method"
   (¬⊢flat-class Ψ ((any_0 ... (string any_fieldspec ...) any_1 ...)
                    (any_1 ... (string any_methodspec ...) any_2 ...)))]

  [(evalo Ψ t_0 T_0)
   (evalo Ψ t_1 T_1)
   (where #t (≠ T_0 T_1))
   ------------------"field-twice"
   (¬⊢flat-class Ψ ((any_0 ...
                     (string_0 t_0)
                     any_1 ...
                     (string_0 t_1)
                     any_2 ...)
                    any_methods))]

  [(where #f (Ψ⊢T≲T Ψ (-> ([x+☠_c t_ci] ...) t_co) (-> ([x+☠_p t_pi] ...) t_po)))
   ;; don't worry about constructors
   (where #t (≠ string_0 "__init__"))
   ------------------"method-incompatible"
   (¬⊢flat-class Ψ
                 (any_fields
                  (any_0 ...
                   (string_0 ([x+☠_c t_ci] ...) t_co)
                   any_1 ...
                   (string_0 ([x+☠_p t_pi] ...) t_po)
                   any_2 ...)))])


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢class-member I I I)
  #:contract (ΨΓ⊢class-member Ψ Γ class-member)

  ;; Is this class-member well-formed under the environment Γ?

  [-------------------------
   (ΨΓ⊢class-member Ψ Γ (field string t))]

  [(evalo Ψ t_ret T_ret)
   (ΨΓ⊢s⇐T Ψ Γ s T_ret) ...
   -------------------------
   (ΨΓ⊢class-member Ψ Γ (method string_method x_self ((x_arg t_arg) ...) t_ret s ...))])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢e⇐T (base-Ψ)
           ((x (lookup (base-Ψ) int)))
           42
           (lookup (base-Ψ) int))))


(define-judgment-form SP-statics
  #:mode (ΨΓ⊢e⇐T I I I I)
  #:contract (ΨΓ⊢e⇐T Ψ Γ e T)
  ;; Is e well-formed under Γ and usable as a t?

  [(ΨΓ⊢e⇒T Ψ Γ e T_1)
   (Ψ⊢T≲T Ψ T_1 T_2)
   ------------------------ "switch"
   (ΨΓ⊢e⇐T Ψ Γ e T_2)]
  )

(define-judgment-form SP-statics
  #:mode (constructor-ofo I I O)
  #:contract (constructor-ofo Ψ T ([x+☠_arg t_arg] ...))

  [---------------
   (constructor-ofo Ψ
                    (the-object-class)
                    ())]

  [---------------
   (constructor-ofo Ψ
                    (generic "CheckedDict" T_key T_val)
                    ([☠ dynamic]))]

  [(where #f (member "__init__" (string_method ...)))
   (evalo Ψ x_parent T)
   (constructor-ofo Ψ T ([x+☠_arg t_arg] ...))
   ---------------
   (constructor-ofo
    Ψ
    (class any_self (x_parent)
      any_fields-spec
      ((string_method ([x+☠_mtd t_mtdarg] ...) t_mthret) ...))
    ([x+☠_arg t_arg] ...))]

  [---------------
   (constructor-ofo
    Ψ
    (class any_self (x_parent)
      any_fields-spec
      (any_method-1 ...
       ("__init__" ([x+☠_arg t_arg] ...) t_ret)
       any_method-2 ...))
    ([x+☠_arg t_arg] ...))])

(define-judgment-form SP-statics
  #:mode (as-fun I I I O)
  #:contract (as-fun Ψ T number T)

  [(constructor-ofo Ψ T ([x+☠_arg t_arg] ...))
   (where #t (= (len ([x+☠_arg t_arg] ...)) number))
   ------------------------ "class-as-fun"
   (as-fun Ψ
           (class T)
           number
           (-> ([x+☠_arg t_arg] ...) (quote T)))]

  [(where #t (= (len (t_arg ...)) number))
   ------------------------ "fun-as-fun"
   (as-fun Ψ
           (-> ([x+☠_arg t_arg] ...) t_ret)
           number
           (-> ([x+☠_arg t_arg] ...) t_ret))]

  [(where (t_arg ...) ,(make-list (term number) (term dynamic)))
   (where t_ret dynamic)
   ------------------------ "dyn-as-fun"
   (as-fun Ψ dynamic number (-> ([☠ t_arg] ...) t_ret))]
)

(define-metafunction SP-statics
  lookup-member : Ψ T string -> T
  [(lookup-member Ψ T string)
   T_mem
   (where (([string_field t_field] ...)
           ([string_method ([x+☠ t_arg] ...) t_ret] ...))
          (flatten-class Ψ T))
   (where t_mem
          (lookup ([string_field t_field] ...
                   [string_method '(-> ([x+☠ t_arg] ...) t_ret)] ...
                   [string dynamic])
                  string))
   (judgment-holds (evalo Ψ t_mem T_mem))])

(define-judgment-form SP-statics
  #:mode (as-subscriptable I I O O)
  #:contract (as-subscriptable Ψ T T T)

  [---
   (as-subscriptable Ψ dynamic dynamic dynamic)]

  [(where T_getitem (lookup-member Ψ T_map "__getitem__"))
   (as-fun Ψ T_getitem 1 (-> ([any t_key]) t_val))
   (evalo Ψ t_key T_key)
   (evalo Ψ t_val T_val)
   ---
   (as-subscriptable Ψ T_map T_key T_val)])

(define-judgment-form SP-statics
  #:mode (ΨΓ⊢e⇒T I I I O)
  #:contract (ΨΓ⊢e⇒T Ψ Γ e T)
  ;; Is e well-formed under Γ and usable as a T?

  [(evalo Ψ (subscript x (tuple-syntax t ...)) (generic string T ...))
   ------------------------ "generic"
   (ΨΓ⊢e⇒T Ψ Γ (subscript x (tuple-syntax t ...)) (class (generic string T ...)))]

  [(lookupo Γ x T)
   ------------------------ "variable"
   (ΨΓ⊢e⇒T Ψ Γ x T)]

  [----------------------- "None"
   (ΨΓ⊢e⇒T Ψ Γ None (prim-class "None-class"))]

  [----------------------- "integer"
   (ΨΓ⊢e⇒T Ψ Γ integer (lookup (base-Ψ) int))]

  [----------------------- "boolean"
   (ΨΓ⊢e⇒T Ψ Γ boolean (lookup (base-Ψ) bool))]

  [----------------------- "string"
   (ΨΓ⊢e⇒T Ψ Γ string (lookup (base-Ψ) str))]

  [(ΨΓ⊢e⇐T Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐T Ψ Γ e_value dynamic) ...
   ----------------------- "PyDict"
   (ΨΓ⊢e⇒T Ψ Γ (dict-syntax [e_key e_value] ...) (prim-class "dict"))]
  
  [(ΨΓ⊢e⇒T Ψ Γ e_map T_map)
   (as-subscriptable Ψ T_map T_key T_val)
   (ΨΓ⊢e⇐T Ψ Γ e_key T_key)
   ----------------------- "subscription"
   (ΨΓ⊢e⇒T Ψ Γ (subscript e_map e_key) T_val)]

  [(ΨΓ⊢e⇒T Ψ Γ e_ins T_ins)
   (where T_mem (lookup-member Ψ T_ins string_mem))
   ----------------------- "attribute / access member"
   (ΨΓ⊢e⇒T Ψ Γ (attribute e_ins string_mem) T_mem)]

  [(ΨΓ⊢e⇒T Ψ Γ e_fun T_fun)
   (as-fun Ψ T_fun (len (e_arg ...)) (-> ([x+☠_arg t_arg] ...) t_ret))
   (evalo Ψ t_arg T_arg) ...
   (evalo Ψ t_ret T_ret)
   (ΨΓ⊢e⇐T Ψ Γ e_arg T_arg) ...
   ----------------------- "function application"
   (ΨΓ⊢e⇒T Ψ Γ (e_fun e_arg ...) T_ret)]
  )

; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))


; (lookup Γ x) retrieves x's type from Γ
(module+ test
  (test-equal (term (lookup ((x int) (x None) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x None) (y int)) y)) (term int)))

