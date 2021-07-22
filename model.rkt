#lang racket
(require redex)
(require redex-abbrevs)

(define-language StaticPython
  ;; program
  (p (s ...))
  ;; statements
  (s (return e)
     (declare x t)
     pass
     (class x_cls x_sup s ...)
     (def (x_fun (x_arg t_arg) ...) t_ret s ...)
     (if e_cnd
         (s_thn ...)
         (s_els ...))
     e)
  (i number)
  (e x
     i
     None
     (is-not e_0 e_1)
     (+ e_0 e_1)
     (x_fun e_arg ...))
  (t dynamic
     (union t_0 t_1)
     int
     bool
     None
     ;; the strings represent path (e.g. "__main__")
     (function (string ...) x_fun (t_arg ...) t_ret)
     (class x_self x_super
       ((x_fid t_fid) ...)
       ((x_mtd (function (string ...) x_mtd (t_arg ...) t_ret)) ...))
     ⊥
     x)
  (x variable-not-otherwise-mentioned))


(define-extended-language StaticPython-tc StaticPython
  (Γ ::= ((x t) ...)))

(module+ test
  (define test_compat_override
    (term
     ((class C object
        (declare x int))
      (class D C
        (declare x int)))))
  (define test_incompat_override
    (term
     ((class C object
        (declare x int))
      (class D C
        (def (x (self D)) dynamic
          pass)))))
  (test-match StaticPython p test_compat_override)
  (test-match StaticPython p test_incompat_override)
  (check-judgment-holds*
   (⊢p ,test_compat_override))
  (check-not-judgment-holds*
   (⊢p ,test_incompat_override)))

(define-judgment-form StaticPython-tc
  #:mode (⊢p I)
  #:contract (⊢p p)
  [(where Γ (collect-defs s ...))
   (Γ⊢s:t Γ s ⊥) ...
   ------------------------ "collect-def"
   (⊢p (s ...))])

(module+ test
  (check-judgment-holds*
   (⊢p ())
   (⊢p ((declare x int)))
   (⊢p (pass))
   (⊢p ((class C object)))
   (⊢p ((def (f (n int)) int
          (return n))))
   (⊢p ((if 0 (pass) (pass))))
   (⊢p (42)))
  ;; more class tests
  (check-judgment-holds*
   (⊢p ((class C object
          (declare x int))))))


(define-judgment-form StaticPython-tc
  #:mode (Γ⊢s*:t I I I)
  #:contract (Γ⊢s*:t Γ (s ...) t)
  [(where ((x_new t_new) ...) (collect-defs s ...))
   (where Γ_sub (extend Γ (x_new t_new) ...))
   (Γ⊢s:t Γ_sub s t) ...
   ------------------------ "collect-def"
   (Γ⊢s*:t Γ (s ...) t)])





(define-judgment-form StaticPython-tc
  #:mode (Γ⊢s:t I I I)
  #:contract (Γ⊢s:t Γ s t)

  
  [(Γ⊢s*:t (extend Γ (x_arg t_arg) ...)
           (s ...)
           t_ret)
   ------------------------ "def"
   (Γ⊢s:t Γ
          (def (x_fun (x_arg t_arg) ...) t_ret s ...)
          t)]


  [(Γ⊢s*:t Γ (s ...) ⊥)
   ------------------------ "class"
   ;; TODO this rule isn't quite right because of Py's weird scoping rules.
   (Γ⊢s:t Γ
          (class x_cls x_sup s ...)
          t)]

  
  [(Γ⊢e⇐t Γ e_cnd dynamic)
   (Γ⊢s*:t Γ (s_thn ...) t)
   (Γ⊢s*:t Γ (s_els ...) t)
   ------------------------ "if"
   (Γ⊢s:t Γ
          (if e_cnd
              (s_thn ...)
              (s_els ...))
          t)]


  [------------------------ "pass"
   (Γ⊢s:t Γ pass t)]
  

  [(≲ (lookup Γ x_var) t_var)
   ------------------------ "declare"
   (Γ⊢s:t Γ (declare x_var t_var) t)]


  [(Γ⊢e⇐t Γ e t)
   ------------------------ "return"
   (Γ⊢s:t Γ (return e) t)]
  

  [(Γ⊢e⇐t Γ e dynamic)
   ------------------------ "end-⊥"
   (Γ⊢s:t Γ e ⊥)]


  [(Γ⊢e⇐t Γ e dynamic)
   ------------------------ "end-None"
   (Γ⊢s:t Γ e None)])

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

(define-metafunction StaticPython-tc
  collect-defs : s ... -> Γ
  ;; collect declarations in the block (s ...)
  
  [(collect-defs) ()]
  
  [(collect-defs (declare x t) s_rst ...)
   (extend (collect-defs s_rst ...)
           (x t))]
  
  [(collect-defs (def (x_fun (x_arg t_arg) ...) t_ret s_bdy ...)
                 s_rst ...)
   (extend (collect-defs s_rst ...)
           (x_fun (function ("__main__") x_fun (t_arg ...) t_ret)))]

  
   ;; TODO 
  [(collect-defs (class x_cls x_sup s_cls ...)
                 s_rst ...)
   (extend (collect-defs s_rst ...)
           (x_cls (class x_cls x_sup Γ_fld Γ_mtd)))
   (where Γ_cls (collect-defs s_cls ...))
   (where Γ_fld (declared-fields Γ_cls))
   (where Γ_mtd (declared-methods Γ_cls))]

  [(collect-defs s_fst s_rst ...)
   (collect-defs s_rst ...)])

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