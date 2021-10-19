#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))

(define-extended-language SP-tc StaticPython
  ;; class environment
  (Ψ ((x T) ...))
  ;; types
  (t .... (quote T))
  ;; type values (classes and dynamic)
  (T dynamic
     (base-class)
     (prim-class string)
     (prim-generic string)
     (generic string T ...)
     (-> (t ...) t)
     (class x_self (x_parent ...)
       ((string_field t_field) ...)
       ((string_method (t_arg ...) t_ret) ...))
     ;; classes themselves, useful in instance construction
     (type T))
  ;; type environment
  (Γ ((x T) ...))
  ;; a handy concept that is useful when I want to check well-formness of classes
  (flat-class (((string_field t_field) ...)
               ((string_method (t_arg ...) t_ret) ...)))
  (flat-class+☠ flat-class ☠))

(define-metafunction SP-tc
  base-Ψ : -> Ψ
  [(base-Ψ) ((object (base-class))
             (float
              (class float (object)
                ()
                (("__init__" (dynamic) None)
                 ("__add__" (float float) float))))
             (int (class int (float) () ()))
             (bool (class bool (int) () ()))
             (str (prim-class "str"))
             (dict (prim-class "dict"))
             (Callable (prim-generic "Callable")))])

(module+ test
  (check-not-judgment-holds*
   (Ψ⊢t≲t () bool bool)
   (Ψ⊢t≲t () int int)
   (Ψ⊢t≲t () str str)
   (Ψ⊢t≲t () bool int)
   (Ψ⊢t≲t () bool float))
  (check-judgment-holds*
   (Ψ⊢t≲t (base-Ψ) bool bool)
   (Ψ⊢t≲t (base-Ψ) int int)
   (Ψ⊢t≲t (base-Ψ) str str)
   (Ψ⊢t≲t (base-Ψ) None None)
   (Ψ⊢t≲t (base-Ψ) int dynamic)
   (Ψ⊢t≲t (base-Ψ) dynamic int)
   (Ψ⊢t≲t (base-Ψ) bool int)
   (Ψ⊢t≲t (base-Ψ) bool float)
   (Ψ⊢t≲t (base-Ψ)
          (subscript Callable (tuple-syntax int bool))
          (subscript Callable (tuple-syntax int int)))
   (Ψ⊢t≲t (base-Ψ)
          (subscript Callable (tuple-syntax int int))
          (subscript Callable (tuple-syntax bool int))))
  (check-judgment-holds*
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ())))
          D C)
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ())))
          (subscript Callable (tuple-syntax int D))
          (subscript Callable (tuple-syntax int C)))
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ())))
          (subscript Callable (tuple-syntax C int))
          (subscript Callable (tuple-syntax D int)))
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ()))
                  (CheckedDict (prim-generic "CheckedDict")))
          (subscript CheckedDict (tuple-syntax int str))
          (subscript CheckedDict (tuple-syntax int str))))
  (check-not-judgment-holds*
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ())))
          C D)
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ())))
          (subscript Callable (tuple-syntax int C))
          (subscript Callable (tuple-syntax int D)))
   (Ψ⊢t≲t (extend (base-Ψ)
                  (C (class C (object) () ()))
                  (D (class D (C) () ())))
          (subscript Callable (tuple-syntax D int))
          (subscript Callable (tuple-syntax C int))))
  )

(define-judgment-form SP-tc
  #:mode (lookupo I I O)
  #:contract (lookupo ((any any) ...) any any)
  [(where #f (member any_key2 (any_key1 ...)))
   ---------------------- "Found"
   (lookupo ((any_key1 any_val1) ...
             (any_key2 any_val2)
             (any_key3 any_val3) ... )
            any_key2
            any_val2)])

(define-judgment-form SP-tc
  #:mode (evalo I I O)
  #:contract (evalo Ψ t T)

  [------------------------- "quotation"
   (evalo Ψ (quote T) T)]

  [------------------------- "dynamic"
   (evalo Ψ dynamic dynamic)]

  [------------------------- "None"
   (evalo Ψ None (prim-class "None"))]

  [(lookupo Ψ x (prim-generic "Callable"))
   ------------------------- "Callable"
   (evalo Ψ
          (subscript x (tuple-syntax t_input ... t_output))
          (-> (t_input ...) t_output))]

  [(lookupo Ψ x (prim-generic "CheckedDict"))
   (evalo Ψ t_1 T_1)
   (evalo Ψ t_2 T_2)
   ------------------------- "CheckedDict"
   (evalo Ψ
          (subscript x (tuple-syntax t_1 t_2))
          (generic "CheckedDict" T_1 T_2))]

  [(lookupo Ψ x T)
   ------------------------- "Lookup"
   (evalo Ψ x T)])

(define-judgment-form SP-tc
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
   (Ψ⊢T≲T Ψ (-> (t_0i ...) t_0o) (-> (t_1i ...) t_1o))]

  [------------------------ "C≲object"
   (Ψ⊢T≲T Ψ T (base-class))]

  [------------------------ "primitive"
   (Ψ⊢T≲T Ψ (prim-class string) (prim-class string))]

  [------------------------ "refl"
   (Ψ⊢T≲T Ψ T T)]

  [(where (class x_self-1 (x_parent-1)
            any_field-spec-1
            any_method-spec-1)
          T_1)
   (where (class x_self-2 (x_parent-2)
            any_field-spec-2
            any_method-spec-2)
          T_2)
   (where #t (≠ x_self-1 x_self-2))
   (lookupo Ψ x_parent-1 T_super)
   (Ψ⊢T≲T Ψ T_super T_2)
   ------------------------ "super"
   (Ψ⊢T≲T Ψ T_1 T_2)]
  )

(define-judgment-form SP-tc
  #:mode (Ψ⊢t≲t I I I)
  #:contract (Ψ⊢t≲t Ψ t t)
  ;; Is it sensible to use a value of type t_0 as as value of type t_1?
  ;; (a.k.a. consistent subtyping)

  [(evalo Ψ t_1 T_1)
   (evalo Ψ t_2 T_2)
   (Ψ⊢T≲T Ψ T_1 T_2)
   ------------------------
   (Ψ⊢t≲t Ψ t_1 t_2)])

(define-metafunction SP-tc
  member : any (any ...) -> boolean
  [(member any_0 (any_1 ... any_0 any_2 ...)) #t]
  [(member any_0 (any_1 ...)) #f])

(define-metafunction SP-tc
  not : boolean -> boolean
  [(not #t) #f]
  [(not #f) #t])

(define-metafunction SP-tc
  len : (any ...) -> number
  [(len (any ...)) ,(length (term (any ...)))])

(define-metafunction SP-tc
  = : any any -> boolean
  [(= any any) #t]
  [(= any_0 any_1) #f])

(define-metafunction SP-tc
  ≠ : any any -> boolean
  [(≠ any_0 any_1) (not (= any_0 any_1))])

(define-metafunction SP-tc
  extend : ((variable any) ...) (variable any) ... -> ((variable any) ...)
  [(extend ((variable_known any_known) ...) (variable_new any_new) ...)
   ((variable_new any_new) ...(variable_known any_known) ...)])

(define-judgment-form SP-tc
  #:mode (⊢flat-class I I)
  #:contract (⊢flat-class Ψ flat-class)

  ;; Is this flat-class well-formed under the class environment K?

  [(where #f (¬⊢flat-class Ψ flat-class))
   -----------------
   (⊢flat-class Ψ flat-class)])

(define-judgment-form SP-tc
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_0 (base-Ψ))
   (where Ψ_1 (collect-imports Ψ_0 import-type ...))
   (where Ψ_2 (collect-clss Ψ_1 s ...))
   (where Ψ Ψ_2)
   (where ((x T) ...) Ψ)
   (Ψ⊢T Ψ T) ...
   (where Γ (collect-defs Ψ s ...))
   (ΨΓ⊢s⇐T Ψ Γ s dynamic) ...
   ------------------------
   (⊢p (import-type ... s ...))])

(module+ test
  (test-equal
   (term (collect-imports ((object (base-class)))))
   (term ((object (base-class))))))

(define-metafunction SP-tc
  collect-imports : Ψ import-type ... -> any
  [(collect-imports Ψ_0) Ψ_0]
  [(collect-imports
    Ψ_0
    (import-from "__static__" (PyDict))
    import-type_0 ...)
   (collect-imports
    (extend Ψ_0 (PyDict (prim-class "dict")))
    import-type_0 ...)]
  [(collect-imports
    Ψ_0
    (import-from "__static__" (CheckedDict))
    import-type_0 ...)
   (collect-imports
    (extend Ψ_0 (CheckedDict (prim-generic "CheckedDict")))
    import-type_0 ...)]
  [(collect-imports
    Ψ_0
    (import-from "__static__" (cast))  ;; TODO: cast are skipped
    import-type_0 ...)
   (collect-imports
    Ψ_0
    import-type_0 ...)]
  [(collect-imports
    Ψ_0
    (import-from string_0 (x_0 x_1 x_2 ...))
    import-type_0 ...)
   (collect-imports
    Ψ_0
    (import-from string_0 (x_0))
    (import-from string_0 (x_1 x_2 ...))
    import-type_0 ...)]
  [(collect-imports any ...)
   #f])

(define-metafunction SP-tc
  collect-clss : Ψ s ... -> Ψ
  ;; What are the defined classes?
  [(collect-clss Ψ_0) Ψ_0]
  [(collect-clss Ψ_0 (class x_child (x_parent) class-member ...) s ...)
   Ψ_1
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where T (class x_child (x_parent) any_fields any_methods))
   (where Ψ_1 (collect-clss (extend Ψ_0 (x_child T)) s ...))]
  [(collect-clss Ψ_0 s_1 s_2 ...)
   (collect-clss Ψ_0 s_2 ...)])


(define-metafunction SP-tc
  collect-defs : Ψ s ... -> Γ
  ;; What are the classes and variables defined at the top level?
  [(collect-defs Ψ) ()]

  [(collect-defs Ψ (define/assign x t e) s ...)
   (extend (collect-defs Ψ s ...)
           (x T))
   (judgment-holds (evalo Ψ t T))]

  [(collect-defs Ψ (define/assign x e) s ...)
   (extend (collect-defs Ψ s ...)
           (x dynamic))]

  [(collect-defs Ψ (def x_fun ([x_arg t_arg] ...) t_ret s_body ...) s ...)
   (extend (collect-defs Ψ s ...)
           (x_fun (-> (t_arg ...) t_ret)))]

  ;; ignore the define/assign if the lhs is not a variable
  [(collect-defs Ψ (define/assign e t e) s ...)
   (collect-defs Ψ s ...)]

  [(collect-defs Ψ s_fst s_rst ...)
   (collect-defs Ψ s_rst ...)])

(define-judgment-form SP-tc
  #:mode (constructor-ofo I I O)
  #:contract (constructor-ofo Ψ T (T ...))

  [---------------
   (constructor-ofo Ψ (base-class) ())]

  [---------------
   (constructor-ofo Ψ (prim-class string) (dynamic))]

  [---------------
   (constructor-ofo Ψ
                    (generic "CheckedDict" T_key T_val)
                    (dynamic))]

  [(where #f (member "__init__" (string_method ...)))
   (lookupo Ψ x_parent T)
   (constructor-ofo Ψ T (T_arg ...))
   ---------------
   (constructor-ofo
    Ψ
    (class x_self (x_parent)
      any_fields-spec
      ((string_method (t_arg ...) t_ret) ...))
    (T_arg ...))]

  [(evalo Ψ t_arg T_arg) ...
   ---------------
   (constructor-ofo
    Ψ
    (class x_self (x_parent)
      any_fields-spec
      (any_method-1 ...
       ("__init__" (t_arg ...) t_ret)
       any_method-2 ...))
    (T_arg ...))])

(define-judgment-form SP-tc
  #:mode (flatten-classo I I O)
  #:contract (flatten-classo Ψ T flat-class)

  [---------------------
   (flatten-classo
    Ψ
    (base-class)
    (()
     (("__init__" () None))))]

  [---------------------
   (flatten-classo Ψ
                   (prim-class string)
                   (()
                    (("__init__" () None))))]

  [(flatten-classo
    Ψ
    (lookup Ψ x_parent)
    ((any_field ...)
     (any_method ...)))
   -----------------------
   (flatten-classo
    Ψ
    (class x_self (x_parent)
      (any_new-field ...)
      (any_new-method ...))
    ((any_new-field ... any_field ...)
     (any_new-method ... any_method ...)))])

(define-judgment-form SP-tc
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


(define-metafunction SP-tc
  collect-mems : class-member ... -> (((string_field t_field) ...)
                                      ((string_method (t_arg ...) t_ret) ...))
  ;; What are the fields and methods defined in this class?

  [(collect-mems) (()
                   ())]

  [(collect-mems (field string t) class-member ...)
   (((string t) any_field ...)
    (any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))]

  [(collect-mems (method string_method x_self ((x_arg t_arg) ...) t_ret s ...) class-member ...)
   ((any_field ...)
    ((string_method (t_arg ...) t_ret) any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))])

(module+ test
  (check-judgment-holds*
   (ΨΓ⊢s⇐T (base-Ψ)
           ((x (lookup (base-Ψ) int)))
           (define/assign x int 42)
           dynamic)))

(define-judgment-form SP-tc
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

  [(ΨΓ⊢e⇒T Ψ Γ e_lhs T)
   (ΨΓ⊢e⇐T Ψ Γ e_rhs T)
   (side-condition (not ,(redex-match? SP-tc x (term e_lhs))))
   ------------------------ "define/assign-synth"
   (ΨΓ⊢s⇐T Ψ Γ (define/assign e_lhs e_rhs) _)]


  [(evalo Ψ t_arg T_arg) ...
   (evalo Ψ t_ret T_ret)
   (where ([x_loc T_loc] ...)
          (collect-defs Ψ s ...))
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
   (ΨΓ⊢s⇐T Ψ Γ (class x_cls (x_sup) class-member ...) _)]


  [(ΨΓ⊢e⇐T Ψ Γ e dynamic)
   ------------------------ "exp"
   (ΨΓ⊢s⇐T Ψ Γ e _)])


(define-judgment-form SP-tc
  #:mode (¬⊢flat-class I I)
  #:contract (¬⊢flat-class Ψ flat-class)

  ;; Is this flat-class ill-formed under the class environment K?

  [------------------"field-and-method"
   (¬⊢flat-class Ψ ((any_0 ... (string t_field) any_1 ...)
                    (any_1 ... (string (t_arg ...) t_ret) any_2 ...)))]

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

  [(where #f (Ψ⊢T≲T Ψ (-> (t_ci ...) t_co) (-> (t_pi ...) t_po)))
   ;; don't worry about constructors
   (where #t (≠ string_0 "__init__"))
   ------------------"method-incompatible"
   (¬⊢flat-class Ψ
                 (any_fields
                  (any_0 ...
                   (string_0 (t_ci ...) t_co)
                   any_1 ...
                   (string_0 (t_pi ...) t_po)
                   any_2 ...)))])


(define-judgment-form SP-tc
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


(define-judgment-form SP-tc
  #:mode (ΨΓ⊢e⇐T I I I I)
  #:contract (ΨΓ⊢e⇐T Ψ Γ e T)
  ;; Is e well-formed under Γ and usable as a t?

  [(ΨΓ⊢e⇒T Ψ Γ e T_1)
   (Ψ⊢T≲T Ψ T_1 T_2)
   ------------------------ "switch"
   (ΨΓ⊢e⇐T Ψ Γ e T_2)]
  )

(define-judgment-form SP-tc
  #:mode (as-fun I I O)
  #:contract (as-fun T number T)

  [(where #t (= (len (t_arg ...)) number))
   ------------------------ "fun-as-fun"
   (as-fun (-> (t_arg ...) t_ret) number (-> (t_arg ...) t_ret))]

  [(where (t_arg ...) ,(make-list (term number) (term dynamic)))
   (where t_ret dynamic)
   ------------------------ "dyn-as-fun"
   (as-fun dynamic number (-> (t_arg ...) t_ret))]
)

(define-judgment-form SP-tc
  #:mode (ΨΓ⊢e⇒T I I I O)
  #:contract (ΨΓ⊢e⇒T Ψ Γ e T)
  ;; Is e well-formed under Γ and usable as a t?

  [(lookupo Ψ x T)
   ------------------------ "ground-class"
   (ΨΓ⊢e⇒T Ψ Γ x (type T))]

  [(evalo Ψ (subscript x (tuple-syntax t ...)) (generic string T ...))
   ------------------------ "generic"
   (ΨΓ⊢e⇒T Ψ Γ (subscript x (tuple-syntax t ...)) (type (generic string T ...)))]

  [(lookupo Γ x T)
   ------------------------ "variable"
   (ΨΓ⊢e⇒T Ψ Γ x T)]

  [----------------------- "None"
   (ΨΓ⊢e⇒T Ψ Γ None (prim-class "None"))]

  [----------------------- "integer"
   (ΨΓ⊢e⇒T Ψ Γ integer (lookup (base-Ψ) int))]

  [----------------------- "boolean"
   (ΨΓ⊢e⇒T Ψ Γ boolean (lookup (base-Ψ) bool))]

  [----------------------- "string"
   (ΨΓ⊢e⇒T Ψ Γ string (lookup (base-Ψ) str))]

  [(ΨΓ⊢e⇐T Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐T Ψ Γ e_value dynamic) ...
   ----------------------- "PyDict"
   (ΨΓ⊢e⇒T Ψ Γ (dict-syntax (e_key e_value) ...) (prim-class "dict"))]

  [(ΨΓ⊢e⇒T Ψ Γ e_map (prim-class "dict"))
   (ΨΓ⊢e⇐T Ψ Γ e_key dynamic)
   ----------------------- "lookup-PyDict"
   (ΨΓ⊢e⇒T Ψ Γ (subscript e_map e_key) dynamic)]

  [(ΨΓ⊢e⇒T Ψ Γ e_map (generic "CheckedDict" T_key T_val))
   (ΨΓ⊢e⇐T Ψ Γ e_key T_key)
   ----------------------- "lookup-CheckedDict"
   (ΨΓ⊢e⇒T Ψ Γ (subscript e_map e_key) T_val)]

  [(ΨΓ⊢e⇒T Ψ Γ e_cls (type T_cls))
   (constructor-ofo Ψ T_cls (T_arg ...))
   (side-condition
    (= (len (T_arg ...))
       (len (e_arg ...))))
   (ΨΓ⊢e⇐T Ψ Γ e_arg T_arg) ...
   ----------------------- "class construction"
   (ΨΓ⊢e⇒T Ψ Γ (e_cls e_arg ...) T_cls)]

  [(ΨΓ⊢e⇒T Ψ Γ e_ins T_ins)
   (flatten-classo Ψ T_ins flat-class)
   (where (([string_field t_field] ...)
           ([string_method (t_arg ...) t_ret] ...))
          flat-class)
   (lookupo ([string_field t_field] ...
             [string_method (quote (-> (t_arg ...) t_ret))] ...
             [string_mem dynamic])
            string_mem t_mem)
   (evalo Ψ t_mem T_mem)
   ----------------------- "access member"
   (ΨΓ⊢e⇒T Ψ Γ (attribute e_ins string_mem) T_mem)]

  [(ΨΓ⊢e⇒T Ψ Γ e_fun T_fun)
   (as-fun T_fun (len (e_arg ...)) (-> (t_arg ...) t_ret))
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

(define-metafunction SP-tc
  lookup : ((any any) ...) any -> any
  [(lookup ((any_k1 any_v1) ... (any_k any_v) (any_k2 any_v2) ...) any_k)
   any_v
   (side-condition (term (not (member any_k (any_k1 ...)))))]
  [(lookup any_1 any_2)
   ,(error 'lookup "can't find ~e in ~e" (term any_2) (term any_1))])
