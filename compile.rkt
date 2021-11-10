#lang racket
(require redex)
(require "./statics.rkt")
(provide (all-defined-out))

(define-extended-language SP-compiled SP-statics

  ;; program
  (program- (import-type ... d- s-))

  ;; declaration
  (d- (x ...))

  ;; statements
  (s- (define/assign x e-)
      ;   (define/assign (attribute e string) t e)
      (def x ([x e-] ...) d- s-)
      ;   (class x (t ...) m ...)
      ;   (if e s s)
      (begin s- ...)
      ;   (delete x)
      ;   (delete (attribute e string))
      (return e-)
      (expr e-)
      ;   (claim x t)
      pass
      (assert e-)
      )

  ;; heap labels
  (l ;; special heap addresses reserved by builtins
   builtin)
  ;; builtin constructs
  (builtin string
           (con c)
           (checked-dict l l)
           (attribute builtin string))

  ;   (m (field string t)
  ;      (method string x ([x t] ...) t d s))

  ;; values are just heap addresses
  (v (ref l))

  (e- x
      c
      v
      (tuple-syntax e- ...)
      (set-syntax e- ...)
      (dict-syntax [e- e-] ...)
      (is e- e-)
      (is-not e- e-)
      (if e- e- e-)
      (dynamic-attribute e- string)
      (static-attribute e- string)
      (e- e- ...)
      ;  (reveal-type any ... e)
      ;  (bool-op ob e e)
      (check-isinstance! e- e-)
      ))

(define-metafunction SP-compiled
  compile-program : program -> program-
  [(compile-program (import-type ... d s))
   (import-type ... (compile-d d) (compile-s Ψ_2 Γ_5 Γ_5 ☠ None s))
   (where Ψ_1 (base-Ψ))
   (where Γ_1 (base-Γ))
   (where Γ_2 (collect-imports Γ_1 import-type ...))
   (where ([x any] ...) d)
   (where Γ_3 (extend Γ_2 [x ☠] ...))
   (where (d_cls d_oth) (split-d d))
   (where (Ψ_2 Γ_4) (define-classes Ψ_1 Γ_3 d_cls))
   ;; then we define other things, functions and ordinary varibles
   (where ([x_var D_var] ...) d_oth)
   (judgment-holds (evalD* Ψ_2 Γ_4 (D_var ...) (T_var ...)))
   (where Γ_5 (update Γ_4 [x_var T_var] ...))])

(define-metafunction SP-compiled
  compile-d : d -> d-
  [(compile-d ([x D] ...)) (x ...)])

(define-metafunction SP-compiled
  get-e : (e- T) -> e-
  [(get-e (e- T)) e-])

(define-judgment-form SP-compiled
  #:mode (checkable-type I)
  #:contract (checkable-type T)

  [-------------------------------------
   (checkable-type dynamic)]

  [-------------------------------------
   (checkable-type (instancesof cid))])

(define-judgment-form SP-compiled
  #:mode (Ψ⊢T<:T I I I)
  #:contract (Ψ⊢T<:T Ψ T T)
  [(where #t (= (union Ψ T_src T_tgt) T_tgt))
   --------------------
   (Ψ⊢T<:T Ψ T_src T_tgt)])

(define-metafunction SP-compiled
  maybe-check-isinstance! : Ψ (e- T) T e- -> e-
  [(maybe-check-isinstance! Ψ (e-_ins T_src) T_tgt e-_tgt)
   e-_ins
   (judgment-holds (Ψ⊢T<:T Ψ T_src T_tgt))]
  [(maybe-check-isinstance! Ψ (e-_ins T_src) T_tgt e-_tgt)
   (check-isinstance! e-_ins e-_tgt)
   (judgment-holds (checkable-type T_tgt))])

(define-metafunction SP-compiled
  maybe-check-isinstance-with-T! : Ψ (e- T) T -> e-
  [(maybe-check-isinstance-with-T! Ψ (e-_ins T_src) T_tgt)
   e-_ins
   (judgment-holds (Ψ⊢T<:T Ψ T_src T_tgt))]
  [(maybe-check-isinstance-with-T! Ψ (e-_ins T_src) T_tgt)
   (check-isinstance! e-_ins (ref (l-of-builtin-class T_tgt)))])

(define-metafunction SP-compiled
  compile-s : Ψ Γ Γ T+☠ e- s -> s-
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ e- (define/assign x t e))
   (define/assign x
     (maybe-check-isinstance!
      Ψ
      (compile-e Ψ Γ_dcl Γ_lcl e)
      T
      (compile-t t)))
   (judgment-holds (evalo Ψ Γ_lcl t T))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ e- (def x ([x_arg t_arg] ...) t_ret d s))
   (def x ([x_arg (compile-t t_arg)] ...)
     (compile-d d)
     (compile-s Ψ Γ_bdy Γ_bdy T_ret (compile-t t_ret) s))
   (judgment-holds (evalo Ψ Γ_dcl t_ret T_ret))
   (where (([x_cls any] ...) ([x_var D_var] ...)) (split-d d))
   (judgment-holds (evalD* Ψ Γ_dcl (D_var ...) (T_var ...)))
   (where Γ_bdy (extend Γ_dcl [x_cls dynamic] ... [x_var T_var] ...))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ e- (begin s ...))
   (begin (compile-s Ψ Γ_dcl Γ_lcl T+☠ e- s) ...)]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ e- (expr e))
   (expr (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))]
  [(compile-s Ψ Γ_dcl Γ_lcl T_ret e-_ret (return e))
   (return (maybe-check-isinstance! Ψ (compile-e Ψ Γ_dcl Γ_lcl e) T_ret e-_ret))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ e- (assert e))
   (assert (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ e- pass)
   pass])

(define-metafunction SP-compiled
  compile-t : t -> e-
  [(compile-t x) x]
  [(compile-t dynamic) (ref "object")]
  [(compile-t None) None]
  [(compile-t ((attribute x string) (tuple-syntax t ...)))
   ((dynamic-attribute x string) (tuple-syntax (compile-t t) ...))]
  [(compile-t ((attribute x string) t))
   ((dynamic-attribute x string) (compile-t t))]
  ;; we are not going to check this at runtime, so the result can be anything
  [(compile-t (or-syntax t_1 t_2)) (ref "object")])

(module+ test
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ) [cd (instancesof ("CheckedDict" (instancesof "int")
                                                                                (instancesof "str")))])
                               (extend (base-Γ) [cd (instancesof ("CheckedDict" (instancesof "int")
                                                                                (instancesof "str")))])
                               ((attribute cd "__setitem__") 2 "def")))
              (term [((ref (attribute (checked-dict "int" "str") "__setitem__"))
                      cd
                      (ref (con 2))
                      (ref (con "def")))
                     (instancesof "NoneType")]))
  )
(define-metafunction SP-compiled
  compile-e : Ψ Γ Γ e -> (e- T)
  [(compile-e Ψ Γ_dcl Γ_lcl x)
   (x T)
   (where T (lookup Γ_lcl x))]
  [(compile-e Ψ Γ_dcl Γ_lcl c)
   ((ref (con c)) (instancesof (type-of-c c)))]
  [(compile-e Ψ Γ_dcl Γ_lcl (set-syntax e ...))
   ((set-syntax (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
    (instancesof "set"))]
  [(compile-e Ψ Γ_dcl Γ_lcl (tuple-syntax e ...))
   ((tuple-syntax (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
    (instancesof "tuple"))]
  [(compile-e Ψ Γ_dcl Γ_lcl (dict-syntax [e_key e_val] ...))
   ((dict-syntax [(get-e (compile-e Ψ Γ_dcl Γ_lcl e_key)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_val))] ...)
    (instancesof "dict"))]
  [(compile-e Ψ Γ_dcl Γ_lcl (is e_lft e_rht))
   ((is (get-e (compile-e Ψ Γ_dcl Γ_lcl e_lft)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_rht)))
    (instancesof "bool"))]
  [(compile-e Ψ Γ_dcl Γ_lcl (is-not e_lft e_rht))
   ((is-not (get-e (compile-e Ψ Γ_dcl Γ_lcl e_lft)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_rht)))
    (instancesof "bool"))]
  [(compile-e Ψ Γ_dcl Γ_lcl (if e_cnd e_thn e_els))
   ((if (get-e (compile-e Ψ Γ_dcl Γ_lcl e_cnd))
        (get-e (compile-e Ψ Γ_dcl Γ_lcl e_thn))
        (get-e (compile-e Ψ Γ_dcl Γ_lcl e_els)))
    dynamic)]
  [(compile-e Ψ Γ_dcl Γ_lcl (attribute e string))
   ((dynamic-attribute (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) string)
    dynamic)]
  [(compile-e Ψ Γ_dcl Γ_lcl ((attribute e_obj string_mth) e_arg ...))
   [((ref (attribute l_cls string_mth)) e-_obj (maybe-check-isinstance-with-T! Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...)
    T_ret]
   (where [e-_obj T_obj] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (judgment-holds (is-builtin-class T_obj))
   (where l_cls (l-of-builtin-class T_obj))
   (where (-> ([x+☠_prm T_arg] ...) T_ret) (lookup-member Ψ T_obj string_mth))
   (where #t (= (len (T_arg ...)) (len (e_arg ...))))]
  [(compile-e Ψ Γ_dcl Γ_lcl (e_fun e_arg ...))
   (((get-e (compile-e Ψ Γ_dcl Γ_lcl e_fun)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...)
    dynamic)])


(define-metafunction SP-compiled
  l-of-builtin-class : T -> l
  [(l-of-builtin-class (instancesof string))
   string]
  [(l-of-builtin-class (instancesof ("CheckedDict" T_key T_val)))
   (checked-dict (l-of-builtin-class T_key)
                 (l-of-builtin-class T_val))])

(define-judgment-form SP-compiled
  #:mode (is-builtin-class I)
  #:contract (is-builtin-class T)

  ;; basically, primitive-cid recursively.
  
  [----------------------------------
   (is-builtin-class (instancesof string))]

  [(is-builtin-class T_key)
   (is-builtin-class T_val)
   ----------------------------------
   (is-builtin-class (instancesof ("CheckedDict" T_key T_val)))])

(define-metafunction SP-compiled
  compile-e* : Ψ Γ Γ e ... -> ([e- T] ...)
  [(compile-e* Ψ Γ_dcl Γ_lcl) ()]
  [(compile-e* Ψ Γ_dcl Γ_lcl e_1 e_2 ...)
   ([e-_1 T_1] [e-_2 T_2] ...)
   (where [e-_1 T_1]
          (compile-e Ψ Γ_dcl Γ_lcl e_1))
   (where ([e-_2 T_2] ...)
          (compile-e* Ψ Γ_dcl Γ_lcl e_2 ...))])