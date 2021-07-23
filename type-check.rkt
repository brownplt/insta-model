#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(provide (all-defined-out))


(define-extended-language StaticPython-tc StaticPython
  (Γ ((x T) ...))
  (T t
     C))

(define-judgment-form StaticPython-tc
  #:mode (⊢p I)
  #:contract (⊢p program)
  [(where Γ (collect-defs top-statement ...))
   (Γ⊢top-s:t Γ top-statement) ...
   ------------------------ "collect-def"
   (⊢p (top-statement ...))])


(define-metafunction StaticPython-tc
  collect-defs : top-statement ... -> Γ
  ;; collect declarations in program
  
  [(collect-defs) ()]
  
  [(collect-defs (define x t e) top-statement ...)
   (extend (collect-defs top-statement ...)
           (x t))]
  
  [(collect-defs (class x_child x_parent class-member ...)
                 top-statement ...)
   (extend (collect-defs top-statement ...)
           (x_cls (class x_cls x_sup any_fields any_methods)))
   (where (any_fields any_methods) (collect-mems class-member ...))]

  [(collect-defs top-statement_fst top-statement_rst ...)
   (collect-defs top-statement_rst ...)])

(define-metafunction StaticPython-tc
  collect-mems : class-member ... -> (((x_field t_field) ...)
                                      ((x_method x_self (t_arg ...) t_ret) ...))
  ;; collect declarations in the block (s ...)
  
  [(collect-mems) (()
                   ())]
  
  [(collect-mems (field x t) class-member ...)
   (((x t) any_field ...)
    (any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))]
  
  [(collect-mems (method x_method x_self ((x_arg t_arg) ...) t_ret method-statement ...) class-member ...)
   ((any_field ...)
    ((x_method x_self (t_arg ...) t_ret) any_method ...))
   (where ((any_field ...)
           (any_method ...))
          (collect-mems class-member ...))])

(define-judgment-form StaticPython-tc
  #:mode (Γ⊢top-s:t I I)
  #:contract (Γ⊢top-s:t Γ top-statement)

  
  [(Γ⊢e⇐t Γ e t)
   ------------------------ "declare"
   (Γ⊢top-s:t Γ (define x t e))]



  [------------------------ "pass"
   (Γ⊢top-s:t Γ pass)]


  
  [(Γ⊢cls-mem:t Γ (class-member ...))
   ------------------------ "class"
   ;; TODO this rule isn't quite right because of Py's weird scoping rules.
   (Γ⊢top-s:t Γ (class x_cls x_sup class-member ...))]

 
  [(Γ⊢e⇐t Γ e dynamic)
   ------------------------ "end-⊥"
   (Γ⊢top-s:t Γ e)])


(define-judgment-form StaticPython-tc
  #:mode (Γ⊢s*:t I I I)
  #:contract (Γ⊢s*:t Γ (top-statement ...) t)
  [(where ((x_new t_new) ...) (collect-defs top-statement ...))
   (where Γ_sub (extend Γ (x_new t_new) ...))
   (Γ⊢top-s:t Γ_sub top-statement t) ...
   ------------------------ "collect-def"
   (Γ⊢s*:t Γ (top-statement ...) t)])



(define-judgment-form StaticPython-tc
  #:mode (Γ⊢method-s:t I I I)
  #:contract (Γ⊢method-s:t Γ top-statement t)

  
  [(Γ⊢s*:t (extend Γ (x_arg t_arg) ...)
           (top-statement ...)
           t_ret)
   ------------------------ "def"
   (Γ⊢method-s:t Γ
          (def (x_fun (x_arg t_arg) ...) t_ret top-statement ...)
          t)]


  [(Γ⊢s*:t Γ (top-statement ...) ⊥)
   ------------------------ "class"
   ;; TODO this rule isn't quite right because of Py's weird scoping rules.
   (Γ⊢method-s:t Γ
          (class x_cls x_sup top-statement ...)
          t)]

  
  [(Γ⊢e⇐t Γ e_cnd dynamic)
   (Γ⊢s*:t Γ (top-statement_thn ...) t)
   (Γ⊢s*:t Γ (top-statement_els ...) t)
   ------------------------ "if"
   (Γ⊢method-s:t Γ
          (if e_cnd
              (top-statement_thn ...)
              (top-statement_els ...))
          t)]


  [------------------------ "pass"
   (Γ⊢method-s:t Γ pass t)]
  

  [(≲ (lookup Γ x_var) t_var)
   ------------------------ "declare"
   (Γ⊢method-s:t Γ (declare x_var t_var) t)]


  [(Γ⊢e⇐t Γ e t)
   ------------------------ "return"
   (Γ⊢method-s:t Γ (return e) t)]
  

  [(Γ⊢e⇐t Γ e dynamic)
   ------------------------ "end-⊥"
   (Γ⊢method-s:t Γ e ⊥)]


  [(Γ⊢e⇐t Γ e dynamic)
   ------------------------ "end-None"
   (Γ⊢method-s:t Γ e None)])

(define-judgment-form StaticPython-tc
  #:mode (≲ I I)
  #:contract (≲ t t)
  ;; (≲ t_0 t_1) means it is sensible to use t_0 as t_1
  
  [------------------------ "refl"
   (≲ t t)]
  
  [------------------------ "dynamic-R"
   (≲ t dynamic)]

  [------------------------ "dynamic-L"
   (≲ dynamic t)])

(define-judgment-form StaticPython-tc
  #:mode (Γ⊢e⇐t I I I)
  #:contract (Γ⊢e⇐t Γ e t)
  
  [(≲ (lookup Γ x) t)
   ------------------------ "variable"
   (Γ⊢e⇐t Γ x t)]
  
  [(≲ int t)
   ----------------------- "number"
   (Γ⊢e⇐t Γ i t)]

  [(≲ None t)
   ----------------------- "None"
   (Γ⊢e⇐t Γ None t)]

  [(Γ⊢e⇐t Γ e_0 dynamic)
   (Γ⊢e⇐t Γ e_1 dynamic)
   (≲ bool t)
   ----------------------- "is-not"
   (Γ⊢e⇐t Γ (is-not e_0 e_1) t)]

  [(Γ⊢e⇐t Γ e_0 int)
   (Γ⊢e⇐t Γ e_1 int)
   (≲ int t)
   ----------------------- "+"
   (Γ⊢e⇐t Γ (+ e_0 e_1) t)]

  [(where () (lookup Γ x_fun))
   ----------------------- "application"
   (Γ⊢e⇐t Γ (x_fun e_arg ...) t)])



; (declared-fields Γ) filters Γ so that only field declarations and definitions are kept
(module+ test
  (test-equal (term (declared-fields ((y (function () y () int)) (x int))))
              (term ((x int)))))

(define-metafunction StaticPython-tc
  declared-fields : Γ -> Γ
  [(declared-fields ()) ()]
  [(declared-fields ((x_fst (function (string ...) x_fun (t_arg ...) t_ret))
                    (x_rst t_rst) ...))
   (declared-fields ((x_rst t_rst) ...))]
  [(declared-fields ((x_fst (class x_self x_super
                             ((x_fid t_fid) ...)
                             ((x_mtd (t_arg ...) t_ret) ...)))
                    (x_rst t_rst) ...))
   (declared-fields ((x_rst t_rst) ...))]
  [(declared-fields ((x_fst t_fst) (x_rst t_rst) ...))
   (extend (declared-fields ((x_rst t_rst) ...)) (x_fst t_fst))])

; (declared-methods Γ) filters Γ so that only field declarations and definitions are kept
(module+ test
  (test-equal (term (declared-methods ((y (function () y () int)) (x int))))
              (term ((y (function () y () int))))))

(define-metafunction StaticPython-tc
  declared-methods : Γ -> Γ
  [(declared-methods ()) ()]
  [(declared-methods ((x_fst (function (string ...) x_fst (t_arg ...) t_ret))
                    (x_rst t_rst) ...))
   (extend (declared-methods ((x_rst t_rst) ...)) (x_fst (function (string ...) x_fst (t_arg ...) t_ret)))]
  [(declared-methods ((x_fst t_fst) (x_rst t_rst) ...))
   (declared-methods ((x_rst t_rst) ...))])

; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))
 
(define-metafunction StaticPython-tc
  extend : Γ (x t) ... -> Γ
  [(extend ((x_Γ t_Γ) ...) (x t) ...) ((x t) ...(x_Γ t_Γ) ...)])
 
; (lookup Γ x) retrieves x's type from Γ
(module+ test
  (test-equal (term (lookup ((x int) (x None) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x None) (y int)) y)) (term int)))
 
(define-metafunction StaticPython-tc
  lookup : Γ x -> t
  [(lookup ((x_1 t_1) ... (x t) (x_2 t_2) ...) x)
   t
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term x))])