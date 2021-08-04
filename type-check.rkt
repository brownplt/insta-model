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
       (t_init ...)
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
   (Ψ⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          D C)
   (Ψ⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (int) D)
          (Callable (int) C))
   (Ψ⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (C) int)
          (Callable (D) int)))
  (check-not-judgment-holds*
   (Ψ⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          C D)
   (Ψ⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (int) C)
          (Callable (int) D))
   (Ψ⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
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

  [(Ψ⊢t≲t Ψ t_1i t_0i) ...
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
  ≠ : any any -> boolean
  [(≠ any any) #f]
  [(≠ any_0 any_1) #t])

(define-judgment-form StaticPython-tc
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where Ψ_0 ((object (base-class))))
   (where Ψ_1 (collect-imports Ψ_0 import-type ...))
   (where Ψ_2 (collect-clss Ψ_1 define-class ...))
   (where Γ (collect-defs s ...))
   (ΨΓ⊢s Ψ_2 Γ s) ...
   ------------------------
   (⊢p (import-type ... define-class ... s ...))])

(module+ test
  (test-equal
   (term (collect-imports ((object (base-class)))))
   (term ((object (base-class))))))

(define-metafunction StaticPython-tc
  collect-imports : Ψ import-type ... -> any
  [(collect-imports Ψ_0) Ψ_0]
  [(collect-imports
    Ψ_0
    (import-from string_0 (x_0 x_1 x_2 ...))
    import-type_0 ...)
   (collect-imports
    Ψ_0
    (import-from string_0 (x_0))
    (import-from string_0 (x_1 x_2 ...))
    import-type_0 ...)]
  [(collect-imports
    Ψ_0
    (import-from "__static__" (PyDict))
    import-type_0 ...)
   (collect-imports
    (extend Ψ_0 (PyDict (prim-class PyDict)))
    import-type_0 ...)]
  [(collect-imports any ...)
   #f])

(define-metafunction StaticPython-tc
  collect-clss : Ψ define-class ... -> any
  ;; What are the defined classes?
  [(collect-clss Ψ_0) Ψ_0]
  #;
  ;; when __init__ is defined
  [(collect-clss K_0 (class x_child x_parent class-member ...) define-class ...)
   K_1
   (where (any_fields any_methods)
          (collect-mems class-member ...))
   (where (any_methods-0 ... ("__init__" (t_arg ...) t_ret) any_methods-1 ...)
          any_methods)
   (where C (class x_parent (t_arg ...) any_fields any_methods))
   (judgment-holds (K⊢C K_0 C))
   (where K_1 (collect-clss (extend K_0 (x_child C)) define-class ...))]
  ;; when __init__ is not defined
  [(collect-clss Ψ_0 (class x_child x_parent class-member ...) define-class ...)
   Ψ_1
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where C (class x_parent () any_fields any_methods))
   (judgment-holds (K⊢C Ψ_0 C))
   (where Ψ_1 (collect-clss (extend Ψ_0 (x_child C)) define-class ...))]
  [(collect-clss Ψ_0 define-class ...)
   #f])


(define-metafunction StaticPython-tc
  collect-defs : s ... -> Γ
  ;; What are the classes and variables defined at the top level?  
  [(collect-defs) ()]
  
  [(collect-defs (define x t e) s ...)
   (extend (collect-defs s ...)
           (x t))]

  [(collect-defs s_fst s_rst ...)
   (collect-defs s_rst ...)])


(define-judgment-form StaticPython-tc
  #:mode (K⊢C I I)
  #:contract (K⊢C Ψ C)
  [(⊢flat-class Ψ (flatten-class Ψ C))
   --------------------------------------
   (K⊢C Ψ C)])


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
  #:mode (ΨΓ⊢s I I I)
  #:contract (ΨΓ⊢s Ψ Γ s)

  ;; Is the s well-formed under the type and class environment Γ?
  
  [(ΨΓ⊢e⇐t Ψ Γ e t)
   ------------------------ "declare"
   (ΨΓ⊢s Ψ Γ (define x t e))]


  [------------------------ "pass"
   (ΨΓ⊢s Ψ Γ pass)]

  
  [(Γ⊢class-member Γ class-member) ...
   ------------------------ "class"
   (ΨΓ⊢s Ψ Γ (class x_cls x_sup class-member ...))]

 
  [(ΨΓ⊢e⇐t Ψ Γ e dynamic)
   ------------------------ "exp"
   (ΨΓ⊢s Ψ Γ e)])


(define-metafunction StaticPython-tc
  flatten-class : Ψ C -> flat-class
  [(flatten-class Ψ (base-class))
   (()
    ())]
  [(flatten-class Ψ (class x_parent
                      (t_init ...)
                      (any_new-field ...)
                      (any_new-method ...)))
   ((any_new-field ... any_field ...)
    (any_new-method ... any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (flatten-class Ψ (lookup Ψ x_parent)))])


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

  [(where #f (K⊢t≲t Ψ (Callable (t_ci ...) t_co) (Callable (t_pi ...) t_po)))
   ------------------"method-incompatible"
   (¬⊢flat-class Ψ (any_fields
                  (any_0 ...
                   (string_0 (t_ci ...) t_co)
                   any_1 ...
                   (string_0 (t_pi ...) t_po)
                   any_2 ...)))])


(define-judgment-form StaticPython-tc
  #:mode (⊢flat-class I I)
  #:contract (⊢flat-class Ψ flat-class)

  ;; Is this flat-class well-formed under the class environment K?

  [(where #f (¬⊢flat-class Ψ flat-class))
   -----------------
   (⊢flat-class Ψ flat-class)])


(define-judgment-form StaticPython-tc
  #:mode (Γ⊢class-member I I)
  #:contract (Γ⊢class-member Γ class-member)

  ;; Is this class-member well-formed under the environment Γ?

  [-------------------------
   (Γ⊢class-member Γ (field string t))]

  [;; todo: check method bodies
   -------------------------
   (Γ⊢class-member Γ (method string_method x_self ((x_arg t_arg) ...) t_ret method-statement ...))])


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

  [(ΨΓ⊢e⇐t Ψ Γ e_key dynamic) ...
   (ΨΓ⊢e⇐t Ψ Γ e_value dynamic) ...
   (Ψ⊢t≲t Ψ PyDict t)
   ----------------------- "PyDict"
   (ΨΓ⊢e⇐t Ψ Γ (dict (e_key e_value) ...) t)]

  [(where (class x_parent (t_arg ...) any_fields any_methods) (lookup Ψ x_cls))
   (ΨΓ⊢e⇐t Ψ Γ e_arg t_arg) ...
   (Ψ⊢t≲t Ψ x_cls t)
   ----------------------- "class construction"
   (ΨΓ⊢e⇐t Ψ Γ (x_cls e_arg ...) t)]
  )

; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))
 
(define-metafunction StaticPython-tc
  extend : ((x any) ...) (x any) ... -> ((x any) ...)
  [(extend ((x_known any_known) ...) (x_new any_new) ...) ((x_new any_new) ...(x_known any_known) ...)])
 
; (lookup Γ x) retrieves x's type from Γ
(module+ test
  (test-equal (term (lookup ((x int) (x None) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x None) (y int)) y)) (term int)))
 
(define-metafunction StaticPython-tc
  lookup : ((x any) ...) x -> any
  [(lookup ((x_1 any_1) ... (x any) (x_2 any_2) ...) x)
   any
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term any_2))])
