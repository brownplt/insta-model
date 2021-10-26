#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "statics-basic-definitions.rkt")
(require "statics-utilities.rkt")
(require "statics-builtin.rkt")
(provide (all-defined-out))

(define-judgment-form SP-statics
  #:mode (evalo I I O)
  #:contract (evalo Ψ t T)

  ;; Compute type values from type expressions

  [----
   (evalo Ψ dynamic dynamic)]

  [------------------------- "quotation"
   (evalo Ψ (quote T) T)]

  [------------------------- "None"
   (evalo Ψ None (prim-class "None-class"))]

  [(lookupo Ψ x (prim-generic "Callable"))
   ------------------------- "Callable"
   (evalo Ψ
          (subscript x (tuple-syntax t_input ... t_output))
          (-> ([☠ t_input] ...) t_output))]

  [(lookupo Ψ x (prim-generic "CheckedDict"))
   (evalo Ψ t_1 T_1)
   (evalo Ψ t_2 T_2)
   ------------------------- "CheckedDict"
   (evalo Ψ
          (subscript x (tuple-syntax t_1 t_2))
          (generic "CheckedDict" T_1 T_2))]

  [(lookupo Ψ x T)
   ------------------------- "Lookup"  ;; TODO: this doesn't look right to me. We should use Γ
   (evalo Ψ x T)])

(define-judgment-form SP-statics
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
   (Ψ⊢T≲T Ψ (-> ([x+☠ t_0i] ...) t_0o) (-> ([x+☠ t_1i] ...) t_1o))]

  [------------------------ "C≲object"
   (Ψ⊢T≲T Ψ T (the-object-class))]

  [------------------------ "refl"
   (Ψ⊢T≲T Ψ T T)]

  [(where (class any_self-1 (x_parent-1)
            any_field-spec-1
            any_method-spec-1)
          T_1)
   (where (class any_self-2 (x_parent-2)
            any_field-spec-2
            any_method-spec-2)
          T_2)
   (where #t (≠ any_self-1 any_self-2))
   (lookupo Ψ x_parent-1 T_super)
   (Ψ⊢T≲T Ψ T_super T_2)
   ------------------------ "super"
   (Ψ⊢T≲T Ψ T_1 T_2)]
  )