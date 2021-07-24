#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))


(define-extended-language StaticPython-tc StaticPython
  ;; class environment
  (K ((x C) ...))
  ;; type environment
  (Γ ((x t) ...))
  ;; class
  (C (base-class)
     (class x_parent
       ((x_field t_field) ...)
       ((x_method x_self (t_arg ...) t_ret) ...)))
  ;; a handy concept that is useful when I want to check well-formness of classes
  (flat-class ((x_field ...)
               (x_method ...))))


(define-judgment-form StaticPython-tc
  #:mode (⊢p I)
  #:contract (⊢p program)
  ;; Is program well-formed?
  [(where K (collect-clss ((object (base-class))) define-class ...))
   (where Γ (collect-defs s ...))
   (Γ⊢s Γ s) ...
   ------------------------ "collect-def"
   (⊢p (define-class ... s ...))])


(define-metafunction StaticPython-tc
  collect-clss : K define-class ... -> K
  ;; What are the defined classes?
  [(collect-clss K_0) K_0]
  [(collect-clss K_0 (class x_child x_parent class-member ...) define-class ...)
   K_1
   (where (any_fields any_methods) (collect-mems class-member ...))
   (where C (class x_parent any_fields any_methods))
   (judgment-holds (K⊢C K_0 C))
   (where K_1 (collect-clss (extend K_0 (x_child C)) define-class ...))])


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
  [(⊢flat-class (flatten-class K C))
   --------------------------------------
   (K⊢C K C)])


(define-metafunction StaticPython-tc
  collect-mems : class-member ... -> (((x_field t_field) ...)
                                      ((x_method x_self (t_arg ...) t_ret) ...))
  ;; What are the fields and methods defined in this class?  

  [(collect-mems) (()
                   ())]
  
  [(collect-mems (field x t) class-member ...)
   (((x t) any_field ...)
    (any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))]
  
  [(collect-mems (method x_method x_self ((x_arg t_arg) ...) t_ret s ...) class-member ...)
   ((any_field ...)
    ((x_method x_self (t_arg ...) t_ret) any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))])



(define-judgment-form StaticPython-tc
  #:mode (Γ⊢s I I)
  #:contract (Γ⊢s Γ s)

  ;; Is the s well-formed under the type and class environment Γ?
  
  [(Γ⊢e⇐t Γ e t)
   ------------------------ "declare"
   (Γ⊢s Γ (define x t e))]


  [------------------------ "pass"
   (Γ⊢s Γ pass)]

  
  [(Γ⊢class-member Γ class-member) ...
   ------------------------ "class"
   (Γ⊢s Γ (class x_cls x_sup class-member ...))]

 
  [(Γ⊢e⇐t Γ e dynamic)
   ------------------------ "exp"
   (Γ⊢s Γ e)])


(define-metafunction StaticPython-tc
  flatten-class : K C -> flat-class
  [(flatten-class K (base-class))
   (()
    ())]
  [(flatten-class K (class x_parent
                      ((x_field t_field) ...)
                      ((x_method x_self (t_arg ...) t_ret) ...)))
   ((x_field ... x_all-fields ...)
    (x_method ... x_all-methods ...))
   (where ((x_all-fields ...)
           (x_all-methods ...))
          (flatten-class K (lookup K x_parent)))])


(define-judgment-form StaticPython-tc
  #:mode (¬⊢flat-class I)
  #:contract (¬⊢flat-class flat-class)

  [-----------------
   (¬⊢flat-class ((x_0 ... x x_1 ...)
                  (x_2 ... x x_3 ...)))])


(define-judgment-form StaticPython-tc
  #:mode (⊢flat-class I)
  #:contract (⊢flat-class flat-class)

  [(where #f (¬⊢flat-class flat-class))
   -----------------
   (⊢flat-class flat-class)])


(define-judgment-form StaticPython-tc
  #:mode (Γ⊢class-member I I)
  #:contract (Γ⊢class-member Γ class-member)

  ;; Is this class-member well-formed under the environment Γ?

  [-------------------------
   (Γ⊢class-member Γ (field x t))]

  [#;TODO
   -------------------------
   (Γ⊢class-member Γ (method x_method x_self ((x_arg t_arg) ...) t_ret method-statement ...))])

(module+ test
  (check-judgment-holds*
   (≲ int int)
   (≲ int dynamic)
   (≲ dynamic int)
   (≲ bool int)))

(define-judgment-form StaticPython-tc
  #:mode (≲ I I)
  #:contract (≲ t t)
  ;; Is it sensible to use a value of type t_0 as as value of type t_1?
  
  [------------------------ "refl"
   (≲ t t)]
  
  [------------------------ "dynamic-R"
   (≲ t dynamic)]

  [------------------------ "dynamic-L"
   (≲ dynamic t)]

  [------------------------ "bool<:int"
   (≲ bool int)]

  )

(define-judgment-form StaticPython-tc
  #:mode (Γ⊢e⇐t I I I)
  #:contract (Γ⊢e⇐t Γ e t)
  ;; Is e well-formed under Γ and usable as a t?
  
  [(≲ (lookup Γ x) t)
   ------------------------ "variable"
   (Γ⊢e⇐t Γ x t)]
  
  [(≲ int t)
   ----------------------- "integer"
   (Γ⊢e⇐t Γ i t)]

  [(≲ bool t)
   ----------------------- "boolean"
   (Γ⊢e⇐t Γ b t)])

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