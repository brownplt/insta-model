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
      (def x ([x t] ...) t d s)
      ;   (class x (t ...) m ...)
      ;   (if e s s)
      (begin s- ...)
      ;   (delete x)
      ;   (delete (attribute e string))
      (return e-)
      (expr e-)
      ;   (claim x t)
      ;   pass
      (assert e-)
      )

  ;   (m (field string t)
  ;      (method string x ([x t] ...) t d s))

  ;   ;; type expression
  ;   (t dynamic
  ;      None ;; nonterminal x doesn't cover this because we mentioned None in c
  ;      ((attribute x string) (tuple-syntax t ...))
  ;      ((attribute x string) t)
  ;      (or-syntax t t)
  ;      x)

  (e- x
      c
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
  #:mode (checkable-type I)
  #:contract (checkable-type T)

  [-------------------------------------
   (checkable-type (instancesof cid))])

(define-metafunction SP-compiled
  maybe-check-isinstance! : Ψ (e- T) T e- -> e-
  [(maybe-check-isinstance! Ψ (e-_ins T_src) T_tgt e-_cls)
   e-_ins
   (where #t (= (union Ψ T_src T_tgt) T_tgt))]
  [(maybe-check-isinstance! Ψ (e-_ins T_src) T_tgt e-_cls)
   (check-isinstance! e-_ins e-_cls)])

(define-metafunction SP-compiled
  compile-s : Ψ Γ Γ T+☠ s -> s-
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (define/assign x t e))
   (define/assign x
     (maybe-check-isinstance!
      Ψ
      (compile-e Ψ Γ_dcl Γ_lcl e)
      T
      (get-e (compile-e Ψ Γ_dcl Γ_lcl t))))
   (judgment-holds (evalo Ψ Γ_lcl t T))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (def x ([x_arg t_arg] ...) t_ret d s))
   (def x ([x_arg t_arg] ...) t_ret d (compile-s Ψ Γ_dcl Γ_lcl T+☠ s))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s ...))
   (begin (compile-s Ψ Γ_dcl Γ_lcl T+☠ s) ...)]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (expr e))
   (expr (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (return e))
   (return (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (assert e))
   (assert (get-e (compile-e Ψ Γ_dcl Γ_lcl e)))])

(define-metafunction SP-compiled
  compile-e : Ψ Γ Γ e -> (e- T)
  [(compile-e Ψ Γ_dcl Γ_lcl x)
   (x T)
   (judgment-holds (lookupo Γ_lcl x T))]
  [(compile-e Ψ Γ_dcl Γ_lcl c)
   (c (instancesof (type-of-c c)))]
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
   ;; TODO optimization
   ((dynamic-attribute (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) string)
    dynamic)]
  [(compile-e Ψ Γ_dcl Γ_lcl (e_fun e_arg ...))
   (((get-e (compile-e Ψ Γ_dcl Γ_lcl e_fun)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...)
    dynamic)])