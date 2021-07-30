#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))


(define-extended-language StaticPython-tc StaticPython
  ;; class environment
  (K ((x C) ...))
  ;; class
  (C (base-class)
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
   (K⊢t≲t () bool bool)
   (K⊢t≲t () int int)
   (K⊢t≲t () str str)
   (K⊢t≲t () None None)
   (K⊢t≲t () int dynamic)
   (K⊢t≲t () dynamic int)
   (K⊢t≲t () bool int)
   (K⊢t≲t () (Callable (int) bool) (Callable (int) int))
   (K⊢t≲t () (Callable (int) int) (Callable (bool) int)))
  (check-judgment-holds*
   (K⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          D C)
   (K⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (int) D)
          (Callable (int) C))
   (K⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (C) int)
          (Callable (D) int)))
  (check-not-judgment-holds*
   (K⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          C D)
   (K⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (int) C)
          (Callable (int) D))
   (K⊢t≲t ((C (class object () () ()))
           (D (class C () () ())))
          (Callable (D) int)
          (Callable (C) int)))
  )

(define-judgment-form StaticPython-tc
  #:mode (K⊢t≲t I I I)
  #:contract (K⊢t≲t K t t)
  ;; Is it sensible to use a value of type t_0 as as value of type t_1?
  ;; (a.k.a. consistent subtyping)
  
  [------------------------ "dynamic-R"
   (K⊢t≲t K t dynamic)]

  [------------------------ "dynamic-L"
   (K⊢t≲t K dynamic t)]

  [------------------------ "bool≲int"
   (K⊢t≲t K bool int)]

  [------------------------ "tag-bool"
   (K⊢t≲t K bool bool)]

  [------------------------ "tag-int"
   (K⊢t≲t K int int)]

  [------------------------ "tag-str"
   (K⊢t≲t K str str)]

  [------------------------ "tag-None"
   (K⊢t≲t K None None)]

  [(K⊢t≲t K t_1i t_0i) ...
   (K⊢t≲t K t_0o t_1o)
   ------------------------ "tag-callable"
   (K⊢t≲t K
          (Callable (t_0i ...) t_0o)
          (Callable (t_1i ...) t_1o))]

  [------------------------ "tag-class-object"
   (K⊢t≲t K t object)]

  [------------------------ "tag-class-refl"
   (K⊢t≲t K x x)]

  [(where #t (≠ x_0 x_1))
   (where (any_0 ... (x_0 (class x_0-super any_1 ...)) any_2 ...) K)
   (K⊢t≲t K x_0-super x_1)
   ------------------------ "tag-class-super"
   (K⊢t≲t K
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
  [(where K (collect-clss ((object (base-class))) define-class ...))
   (where Γ (collect-defs s ...))
   (KΓ⊢s K Γ s) ...
   ------------------------ "collect-def"
   (⊢p (define-class ... s ...))])


(define-metafunction StaticPython-tc
  collect-clss : K define-class ... -> any
  ;; What are the defined classes?
  [(collect-clss K_0) K_0]
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
  [(collect-clss K_0 (class x_child x_parent class-member ...) define-class ...)
   K_1
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where C (class x_parent () any_fields any_methods))
   (judgment-holds (K⊢C K_0 C))
   (where K_1 (collect-clss (extend K_0 (x_child C)) define-class ...))]
  [(collect-clss K_0 define-class ...)
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
  #:contract (K⊢C K C)
  [(⊢flat-class K (flatten-class K C))
   --------------------------------------
   (K⊢C K C)])


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
  #:mode (KΓ⊢s I I I)
  #:contract (KΓ⊢s K Γ s)

  ;; Is the s well-formed under the type and class environment Γ?
  
  [(KΓ⊢e⇐t K Γ e t)
   ------------------------ "declare"
   (KΓ⊢s K Γ (define x t e))]


  [------------------------ "pass"
   (KΓ⊢s K Γ pass)]

  
  [(Γ⊢class-member Γ class-member) ...
   ------------------------ "class"
   (KΓ⊢s K Γ (class x_cls x_sup class-member ...))]

 
  [(KΓ⊢e⇐t K Γ e dynamic)
   ------------------------ "exp"
   (KΓ⊢s K Γ e)])


(define-metafunction StaticPython-tc
  flatten-class : K C -> flat-class
  [(flatten-class K (base-class))
   (()
    ())]
  [(flatten-class K (class x_parent
                      (t_init ...)
                      (any_new-field ...)
                      (any_new-method ...)))
   ((any_new-field ... any_field ...)
    (any_new-method ... any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (flatten-class K (lookup K x_parent)))])


(define-judgment-form StaticPython-tc
  #:mode (¬⊢flat-class I I)
  #:contract (¬⊢flat-class K flat-class)

  [------------------"field-and-method"
   (¬⊢flat-class K ((any_0 ... (string t_field) any_1 ...)
                  (any_1 ... (string (t_arg ...) t_ret) any_2 ...)))]

  #;; TODO This check is disabled. Waiting for Carl&Dino's reply
  [------------------"field-twice"
   (¬⊢flat-class ((any_0 ...
                   (string_0 t_0)
                   any_1 ...
                   (string_0 t_1)
                   any_2 ...)
                  any_methods))]

  [(where #f (K⊢t≲t K (Callable (t_ci ...) t_co) (Callable (t_pi ...) t_po)))
   ------------------"method-incompatible"
   (¬⊢flat-class K (any_fields
                  (any_0 ...
                   (string_0 (t_ci ...) t_co)
                   any_1 ...
                   (string_0 (t_pi ...) t_po)
                   any_2 ...)))])


(define-judgment-form StaticPython-tc
  #:mode (⊢flat-class I I)
  #:contract (⊢flat-class K flat-class)

  [(where #f (¬⊢flat-class K flat-class))
   -----------------
   (⊢flat-class K flat-class)])


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
  #:mode (KΓ⊢e⇐t I I I I)
  #:contract (KΓ⊢e⇐t K Γ e t)
  ;; Is e well-formed under Γ and usable as a t?
  
  [(K⊢t≲t K (lookup Γ x) t)
   ------------------------ "variable"
   (KΓ⊢e⇐t K Γ x t)]
  
  [(K⊢t≲t K int t)
   ----------------------- "integer"
   (KΓ⊢e⇐t K Γ integer t)]

  [(K⊢t≲t K bool t)
   ----------------------- "boolean"
   (KΓ⊢e⇐t K Γ boolean t)]

  [(where (class x_parent (t_arg ...) any_fields any_methods) (lookup K x_cls))
   (KΓ⊢e⇐t K Γ e_arg t_arg) ...
   (K⊢t≲t K x_cls t)
   ----------------------- "class construction"
   (KΓ⊢e⇐t K Γ (x_cls e_arg ...) t)]
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
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term x))])
