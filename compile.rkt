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
      (def x ([x checkable-T] ...) d- s-)
      (class x (e- ...) m- ...)
      (if e- s- s-)
      (begin s- ...)
      ;   (delete x)
      ;   (delete (attribute e string))
      (return e-)
      (expr e-)
      ;   (claim x t)
      (assert e-)
      )

  ;; heap labels
  (l ;; special heap addresses reserved by builtins
     builtin)
  ;; builtin constructs
  (builtin string
           cid
           (con c)
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
      (bool-op ob e- e-)
      (let ([x e-] ...) s-)
      ))

(define-metafunction SP-compiled
  compile-program : program -> program-
  [(compile-program (import-type ... d s))
   (import-type ... (compile-d d) (compile-s Ψ_2 Γ_5 Γ_5 ☠ s))
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
  #:mode (Ψ⊢T<:T I I I)
  #:contract (Ψ⊢T<:T Ψ T T)
  [(where #t (= (union Ψ T_src T_tgt) T_tgt))
   --------------------
   (Ψ⊢T<:T Ψ T_src T_tgt)])

(define-metafunction SP-compiled
  maybe-cast! : Ψ (e- T) T -> e-
  [(maybe-cast! Ψ (e-_ins T_src) T_tgt)
   e-_ins
   (judgment-holds (Ψ⊢T<:T Ψ T_src T_tgt))]
  [(maybe-cast! Ψ (e-_ins T_src) checkable-T_tgt)
   (let ([tmp e-_ins])
     (begin
       (compile-check checkable-T_tgt tmp)
       (return tmp)))])

(define-metafunction SP-compiled
  compile-check : checkable-T_tgt e- -> s-
  [(compile-check dynamic e-) (begin)]
  [(compile-check (instancesof cid) e-)
   (assert ((ref "isinstance") e- (ref cid)))]
  [(compile-check ("optional" cid) e-)
   (if (is e- None)
       (begin)
       (compile-check (instancesof cid) e-))])

(module+ test
  (test-equal (term (compile-s (base-Ψ)
                               (extend (base-Γ) [x dynamic])
                               (extend (base-Γ) [x dynamic])
                               ☠
                               (begin
                                 (define/assign x dynamic 2)
                                 (expr ((attribute x "__add__") 3)))))
              (term (begin
                      (define/assign x (ref (con 2)))
                      (expr ((ref (attribute "float" "__add__")) x (ref (con 3))))))))
(define-metafunction SP-compiled
  compile-s : Ψ Γ Γ T+☠ s -> s-
  ;; a big chunk of begin things
  [(compile-s Ψ Γ_dcl Γ_lcl ☠ (begin))
   (begin)]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin))
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ (return None))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s))
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ s)]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin (return e) s ...))
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ (return e))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin (begin s_1 ...) s_2 ...))
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s_1 ... s_2 ...))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠_ret (begin (define/assign x t e) s ...))
   (begin (define/assign x e-)
          (compile-s Ψ
                     Γ_dcl
                     (update Γ_lcl [x T_lcl])
                     T+☠_ret
                     (begin s ...)))
   (where T_tgt (lookup Γ_dcl x))
   (where (e- T_src) (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T_lcl (intersection Ψ T_src T_tgt))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠_ret (begin (if e s_thn s_els) s ...))
   (compile-s Ψ Γ_dcl Γ_lcl T+☠_ret (if e (begin s_thn s ...) (begin s_els s ...)))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s_1 s_2 ...))
   (begin (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_1)
          (compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s_2 ...)))]
  ;; claim are ignored
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (claim x t))
   (begin)]
  ;; define/assign x
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (define/assign x t e))
   (define/assign x
     (maybe-cast!
      Ψ
      (compile-e Ψ Γ_dcl Γ_lcl e)
      T))
   (judgment-holds (evalo Ψ Γ_lcl t T))]
  ;; def
  [(compile-s Ψ Γ_1 Γ_lcl T+☠ (def x ([x_arg t_arg] ...) t_ret d s))
   (def x ([x_arg (lookup Γ_3 x_arg)] ...)
     (compile-d d)
     (compile-s Ψ Γ_3 Γ_3 T_ret s))
   (judgment-holds (evalo Ψ Γ_1 t_ret T_ret))
   (where (([x_cls any] ...) ([x_var D_var] ...)) (split-d d))
   (where Γ_2 (extend Γ_1 [x_cls dynamic] ... [x_var ☠] ...))
   (judgment-holds (evalD* Ψ Γ_2 (D_var ...) (T_var ...)))
   (where Γ_3 (update Γ_2 [x_var T_var] ...))]
  ;; classes
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (class x (t ...) m ...))
   (class x ((get-e (compile-e Ψ Γ_dcl Γ_lcl t)) ...)
     (compile-m Ψ Γ_dcl x m) ...)]
  ;; TODO delete x
  ;; TODO delete attr
  ;; expr
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (expr e))
   (expr (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))]
  ;; pass
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ pass) (begin)]
  ;; return
  [(compile-s Ψ Γ_dcl Γ_lcl T (return e))
   (return (maybe-cast! Ψ (compile-e Ψ Γ_dcl Γ_lcl e) T))]
  ;; assert
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (assert e))
   (assert (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))]
  ;; if TODO need to revise this. if should be more sophisticated
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (if e_cnd s_thn s_els))
   (if (get-e (compile-e Ψ Γ_dcl Γ_lcl e_cnd))
       (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_thn)
       (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_els))])

(define-metafunction SP-compiled
  compile-m : Ψ Γ t m -> m-
  [(compile-m Ψ Γ t_cls (field string t_fld))
   (field string (get-e (compile-e Ψ Γ Γ t_fld)))]
  [(compile-m Ψ Γ t_slf (method string_method x_slf ([x_arg t_arg] ...) t_ret d s))
   (method string_method x_slf ([x_arg e-_arg] ...) e-_ret d- s-)
   (where (def tmp ([x_slf e-_slf] [x_arg e-_arg] ...)
            e-_ret
            d-
            s-)
          (compile-s
           Ψ Γ Γ ☠
           (def tmp ([x_slf t_slf] [x_arg t_arg] ...)
             t_ret
             d
             s)))])

(module+ test
  (test-equal (term (compile-e (base-Ψ) (base-Γ) (base-Γ) ((attribute 2 "__add__") 3)))
              (term [((ref (attribute "float" "__add__"))
                      (ref (con 2))
                      (ref (con 3)))
                     (instancesof "float")]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ) [x dynamic])
                               (extend (base-Γ) [x (instancesof "int")])
                               ((attribute x "__add__") 3)))
              (term [((ref (attribute "float" "__add__"))
                      x
                      (ref (con 3)))
                     (instancesof "float")]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               ((attribute CheckedDict "__getitem__")
                                (tuple-syntax int str))))
              (term [((dynamic-attribute CheckedDict "__getitem__")
                      (tuple-syntax int str))
                     (classitself ("CheckedDict" (instancesof "int")
                                                 (instancesof "str")))]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")]
                                       [Optional (prim-generic "Optional")])
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")]
                                       [Optional (prim-generic "Optional")])
                               ((attribute CheckedDict "__getitem__")
                                (tuple-syntax str
                                              ((attribute Optional "__getitem__") str)))))
              (term [((dynamic-attribute CheckedDict "__getitem__")
                      (tuple-syntax str ((dynamic-attribute Optional "__getitem__") str)))
                     (classitself ("CheckedDict" (instancesof "str")
                                                 ("optional" "str")))]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               (((attribute CheckedDict "__getitem__")
                                 (tuple-syntax int str))
                                (dict-syntax))))
              (term [(((dynamic-attribute CheckedDict "__getitem__")
                       (tuple-syntax int str))
                      (dict-syntax))
                     (instancesof ("CheckedDict" (instancesof "int")
                                                 (instancesof "str")))]))
  )
(define-metafunction SP-compiled
  compile-e : Ψ Γ Γ e -> (e- T)
  ;; variable
  [(compile-e Ψ Γ_dcl Γ_lcl x)
   [x T]
   (where T (lookup Γ_lcl x))]
  ;; constant
  [(compile-e Ψ Γ_dcl Γ_lcl c)
   [(ref (con c))
    (instancesof (type-of-c c))]]
  ;; set literal
  [(compile-e Ψ Γ_dcl Γ_lcl (set-syntax e ...))
   [(set-syntax (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
    (instancesof "set")]]
  ;; tuple literal
  [(compile-e Ψ Γ_dcl Γ_lcl (tuple-syntax e ...))
   [(tuple-syntax (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
    (instancesof "tuple")]]
  ;; dict literal
  [(compile-e Ψ Γ_dcl Γ_lcl (dict-syntax [e_key e_val] ...))
   [(dict-syntax [(get-e (compile-e Ψ Γ_dcl Γ_lcl e_key)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_val))] ...)
    (instancesof "dict")]]
  ;; is
  [(compile-e Ψ Γ_dcl Γ_lcl (is e_lft e_rht))
   [(is (get-e (compile-e Ψ Γ_dcl Γ_lcl e_lft)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_rht)))
    (instancesof "bool")]]
  ;; is-not
  [(compile-e Ψ Γ_dcl Γ_lcl (is-not e_lft e_rht))
   [(is-not (get-e (compile-e Ψ Γ_dcl Γ_lcl e_lft)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_rht)))
    (instancesof "bool")]]
  ;; if  TODO need refinement
  [(compile-e Ψ Γ_dcl Γ_lcl (if e_cnd e_thn e_els))
   ((if (get-e (compile-e Ψ Γ_dcl Γ_lcl e_cnd))
        e-_thn
        e-_els)
    (union Ψ T_thn T_els))
   (where [e-_thn T_thn] (compile-e Ψ Γ_dcl Γ_lcl e_thn))
   (where [e-_els T_els] (compile-e Ψ Γ_dcl Γ_lcl e_els))]
  ;; generic class
  [(compile-e Ψ Γ_dcl Γ_lcl ((attribute x "__getitem__") (tuple-syntax t ...)))
   [(ref cid) (classitself cid)]
   (judgment-holds (evalo Ψ Γ_lcl ((attribute x "__getitem__") (tuple-syntax t ...)) (instancesof cid)))]
  ;; attribute
  [(compile-e Ψ Γ_dcl Γ_lcl (attribute e string))
   [(dynamic-attribute (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) string)
    dynamic]]
  ;; Optimization!!
  ;; TODO debug
  [(compile-e Ψ Γ_dcl Γ_lcl ((attribute e_obj string_mth) e_arg ...))
   [((ref l_mth) e-_obj (maybe-cast! Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...)
    T_ret]
   (where [e-_obj T_obj] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where (instancesof cid) T_obj)
   (where (-> ([x+☠_prm T_arg] ...) T_ret) (lookup-member Ψ T_obj string_mth))
   (where l_mth (lookup-member-label Ψ T_obj string_mth))
   (where #t (= (len (T_arg ...)) (len (e_arg ...))))]
  [(compile-e Ψ Γ_dcl Γ_lcl (e_fun e_arg ...))
   [(e-_fun (maybe-cast! Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...)
    T_ret]
   (where [e-_fun T_fun] (compile-e Ψ Γ_dcl Γ_lcl e_fun))
   (judgment-holds (as-fun Ψ T_fun (len (e_arg ...)) (-> ([x+☠_arg T_arg] ...) T_ret)))])


(define-metafunction SP-compiled
  lookup-member-label : Ψ T string -> l
  ;; TODO: this looks a bit too ad hoc ...
  [(lookup-member-label Ψ (instancesof cid) string)
   (attribute cid string)
   (where C (lookup-class Ψ cid))
   (where (class any_nam
            ;; parents
            cid+dynamic+☠
            ;; fields
            ([string_fld T_fld] ...)
            ;; methods
            ([string_mth ([x+☠_arg T_arg] ...) T_ret] ...))
          C)
   (where (any_1 ... string any_2 ...)
          (string_fld ... string_mth ...))]
  [(lookup-member-label Ψ (instancesof cid_slf) string)
   (lookup-member-label Ψ (instancesof cid_prn) string)
   (where C (lookup-class Ψ cid_slf))
   (where (class any_nam
            ;; parents
            cid_prn
            ;; fields
            ([string_fld T_fld] ...)
            ;; methods
            ([string_mth ([x+☠_arg T_arg] ...) T_ret] ...))
          C)])


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
