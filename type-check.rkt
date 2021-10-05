#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))

(define-extended-language StaticPython-tc StaticPython
  ;; class environment
  (Ψ ((x C) ...))
  ;; class
  (C (base-class)
     (prim-class x)
     (class x_parent
       ((string_field t_field) ...)
       ((string_method (t_arg ...) t_ret) ...)))
  ;; type environment
  (Γ ((x t) ...))
  ;; a handy concept that is useful when I want to check well-formness of classes
  (flat-class (((string_field t_field) ...)
               ((string_method (t_arg ...) t_ret) ...))))

(module+ test
  (check-judgment-holds*
   (Ψ⊢t≲t () bool bool)
   (Ψ⊢t≲t () int int)
   (Ψ⊢t≲t () str str)
   (Ψ⊢t≲t () None None)
   (Ψ⊢t≲t () int dynamic)
   (Ψ⊢t≲t () dynamic int)
   (Ψ⊢t≲t () bool int)
   (Ψ⊢t≲t () (Callable (int) bool) (Callable (int) int))
   (Ψ⊢t≲t () (Callable (int) int) (Callable (bool) int)))
  (check-judgment-holds*
   (Ψ⊢t≲t ((object (base-class))
           (C (class object () ()))
           (D (class C () ())))
          D C)
   (Ψ⊢t≲t ((object (base-class))
           (C (class object () ()))
           (D (class C () ())))
          (Callable (int) D)
          (Callable (int) C))
   (Ψ⊢t≲t ((object (base-class))
           (C (class object () ()))
           (D (class C () ())))
          (Callable (C) int)
          (Callable (D) int)))
  (check-not-judgment-holds*
   (Ψ⊢t≲t ((object (base-class))
           (C (class object () ()))
           (D (class C () ())))
          C D)
   (Ψ⊢t≲t ((object (base-class))
           (C (class object () ()))
           (D (class C () ())))
          (Callable (int) C)
          (Callable (int) D))
   (Ψ⊢t≲t ((object (base-class))
           (C (class object () ()))
           (D (class C () ())))
          (Callable (D) int)
          (Callable (C) int)))
  )

(define-judgment-form StaticPython-tc
  #:mode (Ψ⊢t≲t I I I)
  #:contract (Ψ⊢t≲t Ψ t t)
  ;; Is it sensible to use a value of type t_0 as as value of type t_1?
  ;; (a.k.a. consistent subtyping)
  
  [------------------------ "dynamic-R"
   (Ψ⊢t≲t Ψ t dynamic)]

  [------------------------ "dynamic-L"
   (Ψ⊢t≲t Ψ dynamic t)]

  [------------------------ "bool≲int"
   (Ψ⊢t≲t Ψ bool int)]

  [------------------------ "tag-bool"
   (Ψ⊢t≲t Ψ bool bool)]

  [------------------------ "tag-int"
   (Ψ⊢t≲t Ψ int int)]

  [------------------------ "tag-str"
   (Ψ⊢t≲t Ψ str str)]

  [------------------------ "tag-None"
   (Ψ⊢t≲t Ψ None None)]

  [(side-condition
    (= (len (t_1i ...))
       (len (t_0i ...))))
   (Ψ⊢t≲t Ψ t_1i t_0i) ...
   (Ψ⊢t≲t Ψ t_0o t_1o)
   ------------------------ "tag-callable"
   (Ψ⊢t≲t Ψ
          (Callable (t_0i ...) t_0o)
          (Callable (t_1i ...) t_1o))]

  [------------------------ "tag-class-object"
   (Ψ⊢t≲t Ψ t object)]

  [------------------------ "tag-class-refl"
   (Ψ⊢t≲t Ψ x x)]

  [(where #t (≠ x_0 x_1))
   (where (any_0 ... (x_0 (class x_0-super any_1 ...)) any_2 ...) Ψ)
   (Ψ⊢t≲t Ψ x_0-super x_1)
   ------------------------ "tag-class-super"
   (Ψ⊢t≲t Ψ
          x_0 x_1)]
  )

(define-metafunction StaticPython-tc
  member : any (any ...) -> boolean
  [(member any_0 (any_1 ... any_0 any_2 ...)) #t]
  [(member any_0 (any_1 ...)) #f])

(define-metafunction StaticPython-tc
  not : boolean -> boolean
  [(not #t) #f]
  [(not #f) #t])

(define-metafunction StaticPython-tc
  len : (any ...) -> number
  [(len (any ...)) ,(length (term (any ...)))])

(define-metafunction StaticPython-tc
  = : any any -> boolean
  [(= any any) #t]
  [(= any_0 any_1) #f])

(define-metafunction StaticPython-tc
  ≠ : any any -> boolean
  [(≠ any_0 any_1) (not (= any_0 any_1))])

(define-metafunction StaticPython-tc
  extend : ((x any) ...) (x any) ... -> ((x any) ...)
  [(extend ((x_known any_known) ...) (x_new any_new) ...) ((x_new any_new) ...(x_known any_known) ...)])

(define-judgment-form StaticPython-tc
  #:mode (⊢flat-class I I)
  #:contract (⊢flat-class Ψ flat-class)

  ;; Is this flat-class well-formed under the class environment K?

  [(where #f (¬⊢flat-class Ψ flat-class))
   -----------------
   (⊢flat-class Ψ flat-class)])

(define-judgment-form StaticPython-tc
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_0 ((object (base-class))))
   (where Ψ_1 (collect-imports Ψ_0 import-type ...))
   (where Ψ_2 (collect-clss Ψ_1 s ...))
   (where Ψ Ψ_2)
   (where ((x C) ...) Ψ)
   (Ψ⊢C Ψ C) ...
   (where Γ (collect-defs s ...))
   (ΨΓ⊢s⇐t Ψ Γ s dynamic) ...
   ------------------------
   (⊢p (import-type ... s ...))])

(module+ test
  (test-equal
   (term (collect-imports ((object (base-class)))))
   (term ((object (base-class))))))

(define-metafunction StaticPython-tc
  collect-imports : Ψ import-type ... -> any
  [(collect-imports Ψ_0) Ψ_0]
  [(collect-imports
    Ψ_0
    (import-from "__static__" (PyDict))
    import-type_0 ...)
   (collect-imports
    (extend Ψ_0 (PyDict (prim-class PyDict)))
    import-type_0 ...)]
  [(collect-imports
    Ψ_0
    (import-from "__static__" (CheckedDict))
    import-type_0 ...)
   (collect-imports
    (extend Ψ_0 (CheckedDict (prim-class CheckedDict)))
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

(define-metafunction StaticPython-tc
  collect-clss : Ψ s ... -> Ψ
  ;; What are the defined classes?
  [(collect-clss Ψ_0) Ψ_0]
  [(collect-clss Ψ_0 (class x_child x_parent class-member ...) s ...)
   Ψ_1
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where C (class x_parent any_fields any_methods))
   (where Ψ_1 (collect-clss (extend Ψ_0 (x_child C)) s ...))]
  [(collect-clss Ψ_0 s_1 s_2 ...)
   (collect-clss Ψ_0 s_2 ...)])


(define-metafunction StaticPython-tc
  collect-defs : s ... -> Γ
  ;; What are the classes and variables defined at the top level?  
  [(collect-defs) ()]
  
  [(collect-defs (define/assign x t e) s ...)
   (extend (collect-defs s ...)
           (x t))]

  [(collect-defs (define/assign x e) s ...)
   (extend (collect-defs s ...)
           (x dynamic))]

  ;; ignore the define/assign if the lhs is not a variable
  [(collect-defs (define/assign e t e) s ...)
   (collect-defs s ...)]

  [(collect-defs s_fst s_rst ...)
   (collect-defs s_rst ...)])

(define-metafunction StaticPython-tc
  flatten-class : Ψ C -> flat-class
  [(flatten-class Ψ (base-class))
   (()
    (("__init__" () None)))]
  [(flatten-class Ψ (prim-class x))
   (()
    ;; fix this
    (("__init__" () None)))]
  [(flatten-class Ψ (class x_parent
                      (any_new-field ...)
                      (any_new-method ...)))
   ((any_new-field ... any_field ...)
    (any_new-method ... any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (flatten-class Ψ (lookup Ψ x_parent)))])

(define-judgment-form StaticPython-tc
  #:mode (Ψ⊢C I I)
  #:contract (Ψ⊢C Ψ C)
  ;; Is this class well-formed (not overriding member signatures incorrectly)? 
  [(⊢flat-class Ψ (flatten-class Ψ C))
   --------------------------------------
   (Ψ⊢C Ψ C)])


(define-metafunction StaticPython-tc
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

(define-judgment-form StaticPython-tc
  #:mode (ΨΓ⊢s⇐t I I I I)
  #:contract (ΨΓ⊢s⇐t Ψ Γ s t)

  ;; Is the s well-formed under the type and class environment Γ?

  [(ΨΓ⊢e⇐t Ψ Γ e t_ret)
   ------------------------ "return"
   (ΨΓ⊢s⇐t Ψ Γ (return e) t_ret)]
  
  [(ΨΓ⊢e⇐t Ψ Γ e t)
   ------------------------ "define/assign-check"
   (ΨΓ⊢s⇐t Ψ Γ (define/assign x t e) t_ret)]

  [(ΨΓ⊢e⇒t Ψ Γ e_lhs t)
   (ΨΓ⊢e⇐t Ψ Γ e_rhs t)
   (side-condition (not ,(redex-match? StaticPython-tc x (term e_lhs))))
   ------------------------ "define/assign-synth"
   (ΨΓ⊢s⇐t Ψ Γ (define/assign e_lhs e_rhs) t_ret)]
  

  [(ΨΓ⊢e⇐t Ψ Γ e dynamic)
   ------------------------ "delete"
   (ΨΓ⊢s⇐t Ψ Γ (delete e) t_ret)]


  [------------------------ "pass"
   (ΨΓ⊢s⇐t Ψ Γ pass t_ret)]

  
  [(ΨΓ⊢class-member Ψ Γ class-member) ...
   ------------------------ "class"
   (ΨΓ⊢s⇐t Ψ Γ (class x_cls x_sup class-member ...) t_ret)]

 
  [(ΨΓ⊢e⇐t Ψ Γ e dynamic)
   ------------------------ "exp"
   (ΨΓ⊢s⇐t Ψ Γ e t_ret)])


(define-judgment-form StaticPython-tc
  #:mode (¬⊢flat-class I I)
  #:contract (¬⊢flat-class Ψ flat-class)

  ;; Is this flat-class ill-formed under the class environment K?

  [------------------"field-and-method"
   (¬⊢flat-class Ψ ((any_0 ... (string t_field) any_1 ...)
                    (any_1 ... (string (t_arg ...) t_ret) any_2 ...)))]

  [------------------"field-twice"
   (¬⊢flat-class Ψ ((any_0 ...
                     (string_0 t_0)
                     any_1 ...
                     (string_0 t_1)
                     any_2 ...)
                    any_methods))]

  [(where #f (Ψ⊢t≲t Ψ (Callable (t_ci ...) t_co) (Callable (t_pi ...) t_po)))
   ------------------"method-incompatible"
   (¬⊢flat-class Ψ (any_fields
                  (any_0 ...
                   (string_0 (t_ci ...) t_co)
                   any_1 ...
                   (string_0 (t_pi ...) t_po)
                   any_2 ...)))])


(define-judgment-form StaticPython-tc
  #:mode (ΨΓ⊢class-member I I I)
  #:contract (ΨΓ⊢class-member Ψ Γ class-member)

  ;; Is this class-member well-formed under the environment Γ?

  [-------------------------
   (ΨΓ⊢class-member Ψ Γ (field string t))]

  [(ΨΓ⊢s⇐t Ψ Γ s t_ret) ...
   -------------------------
   (ΨΓ⊢class-member Ψ Γ (method string_method x_self ((x_arg t_arg) ...) t_ret s ...))])


(define-judgment-form StaticPython-tc
  #:mode (ΨΓ⊢e⇐t I I I I)
  #:contract (ΨΓ⊢e⇐t Ψ Γ e t)
  ;; Is e well-formed under Γ and usable as a t?
  
  [(Ψ⊢t≲t Ψ (lookup Γ x) t)
   ------------------------ "variable"
   (ΨΓ⊢e⇐t Ψ Γ x t)]
  
  [(Ψ⊢t≲t Ψ None t)
   ----------------------- "None"
   (ΨΓ⊢e⇐t Ψ Γ None t)]
  
  [(Ψ⊢t≲t Ψ int t)
   ----------------------- "integer"
   (ΨΓ⊢e⇐t Ψ Γ integer t)]

  [(Ψ⊢t≲t Ψ bool t)
   ----------------------- "boolean"
   (ΨΓ⊢e⇐t Ψ Γ boolean t)]

  [(Ψ⊢t≲t Ψ str t)
   ----------------------- "string"
   (ΨΓ⊢e⇐t Ψ Γ string t)]

  [(ΨΓ⊢e⇒t Ψ Γ e_map PyDict)
   (ΨΓ⊢e⇐t Ψ Γ e_key dynamic)
   ----------------------- "lookup-PyDict"
   (ΨΓ⊢e⇐t Ψ Γ (subscript e_map e_key) t)]

  
  [(ΨΓ⊢e⇒t Ψ Γ e_map (subscript CheckedDict (tuple t_key t_val)))
   (ΨΓ⊢e⇐t Ψ Γ e_key t_key)
   (Ψ⊢t≲t Ψ t_val t)
   ----------------------- "lookup-CheckedDict"
   (ΨΓ⊢e⇐t Ψ Γ (subscript e_map e_key) t)]

  [(ΨΓ⊢e⇐t Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐t Ψ Γ e_value dynamic) ...
   (Ψ⊢t≲t Ψ PyDict t)
   ----------------------- "PyDict"
   (ΨΓ⊢e⇐t Ψ Γ (dict (e_key e_value) ...) t)]

  [(ΨΓ⊢e⇐t Ψ Γ e_init PyDict)
   ---------------------- "CheckedDict"
   (ΨΓ⊢e⇐t Ψ Γ ((subscript CheckedDict (tuple t_1 t_2)) e_init) (subscript CheckedDict (tuple t_1 t_2)))]

  [(where
    (any_fields (any_method1 ... ("__init__" (t_arg ...) any_ret) any_method2 ...))
    (flatten-class Ψ (lookup Ψ x_cls)))
   (side-condition
    (= (len (t_arg ...))
       (len (e_arg ...))))
   (ΨΓ⊢e⇐t Ψ Γ e_arg t_arg) ...
   (Ψ⊢t≲t Ψ x_cls t)
   ----------------------- "class construction"
   (ΨΓ⊢e⇐t Ψ Γ (x_cls e_arg ...) t)]
  )

(define-judgment-form StaticPython-tc
  #:mode (ΨΓ⊢e⇒t I I I O)
  #:contract (ΨΓ⊢e⇒t Ψ Γ e t)
  ;; Is e well-formed under Γ and usable as a t?
  
  [------------------------ "variable"
   (ΨΓ⊢e⇒t Ψ Γ x (lookup Γ x))]
  
  [----------------------- "None"
   (ΨΓ⊢e⇒t Ψ Γ None None)]
  
  [----------------------- "integer"
   (ΨΓ⊢e⇒t Ψ Γ integer int)]

  [----------------------- "boolean"
   (ΨΓ⊢e⇒t Ψ Γ boolean bool)]

  [----------------------- "string"
   (ΨΓ⊢e⇒t Ψ Γ string str)]

  [(ΨΓ⊢e⇐t Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐t Ψ Γ e_value dynamic) ...
   ----------------------- "PyDict"
   (ΨΓ⊢e⇒t Ψ Γ (dict (e_key e_value) ...) PyDict)]

  [(ΨΓ⊢e⇐t Ψ Γ e_init PyDict)
   ---------------------- "CheckedDict"
   (ΨΓ⊢e⇒t Ψ Γ ((subscript CheckedDict (tuple t_1 t_2)) e_init) (subscript CheckedDict (tuple t_1 t_2)))]

  [(ΨΓ⊢e⇒t Ψ Γ e_map PyDict)
   (ΨΓ⊢e⇐t Ψ Γ e_key dynamic)
   ----------------------- "lookup-PyDict"
   (ΨΓ⊢e⇒t Ψ Γ (subscript e_map e_key) dynamic)]
  
  [(ΨΓ⊢e⇒t Ψ Γ e_map (subscript CheckedDict (tuple t_key t_val)))
   (ΨΓ⊢e⇐t Ψ Γ e_key t_key)
   ----------------------- "lookup-CheckedDict"
   (ΨΓ⊢e⇒t Ψ Γ (subscript e_map e_key) t_val)]
  )

; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))

 
; (lookup Γ x) retrieves x's type from Γ
(module+ test
  (test-equal (term (lookup ((x int) (x None) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x None) (y int)) y)) (term int)))
 
(define-metafunction StaticPython-tc
  lookup : ((x any) ...) x -> any
  [(lookup ((x_1 any_1) ... (x any) (x_2 any_2) ...) x)
   any
   (side-condition (term (not (member x (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term any_2))])
