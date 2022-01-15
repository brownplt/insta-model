#lang racket
(require redex)
(require "utilities.rkt")
(require "desugar.rkt")
(require "compile.rkt")
(require "runtime.rkt")

;; GOAL: progression and preservation

(define-extended-language SP-conjecture
  SP-dynamics)

(define (reduce gas state)
  (cond
    [(zero? gas) state]
    [else
     (match (apply-reduction-relation red-p state)
       [(list next-state)
        (reduce (sub1 gas) next-state)]
       [(list)
        state])]))

;; transtion the input state until it gets stuck or we reach `number` steps.
(define-judgment-form SP-conjecture
  #:mode (-->ⁿ I I O)
  #:contract (-->ⁿ p number p)
  [(where p_2 ,(reduce (term number) (term p_1)))
   ----------------------------------------------
   (-->ⁿ p_1 number p_2)])

;; Is p a good stuck state? A good stuck state is either
;;   terminate, or error.
(define-judgment-form SP-conjecture
  #:mode (terminate-or-error I)
  #:contract (terminate-or-error p)
  [-----------------
   (terminate-or-error (terminate))]
  [-----------------
   (terminate-or-error (error any))])

;; Is p further reducible?
(define-judgment-form SP-conjecture
  #:mode (reducible I)
  #:contract (reducible p)
  
  [(where (p_2) ,(apply-reduction-relation red-p (term p_1)))
   -----------------
   (reducible p_1)])

(define-judgment-form SP-conjecture
  #:mode (ΨΓΓ⊢e+↝e-:T I I I I O O)
  #:contract (ΨΓΓ⊢e+↝e-:T Ψ Γ Γ e+ e- T)
  [(where [e- T]
          ,(with-handlers ([exn:fail:redex? (λ (_) '(not an [e- T]))])
             (term (compile-e Ψ Γ_dcl Γ_lcl (desugar-e e+)))))
   -----------------------------------------------------------
   (ΨΓΓ⊢e+↝e-:T Ψ Γ_dcl Γ_lcl e+ e- T)])

(define-judgment-form SP-conjecture
  #:mode (⊢program+↝program- I O)
  #:contract (⊢program+↝program- program+ program-)
  [(where program-
          ,(with-handlers ([exn:fail:redex? (λ (_) '(not a program))])
             (term (compile-program (desugar-program program+)))))
   ---------------------------------------------------------------
   (⊢program+↝program- program+ program-)])

(define-judgment-form SP-conjecture
  #:mode (Σ⊢v:T I I I)
  #:contract (Σ⊢v:T Σ v T)

  [(where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where #f ,(equal? (term l_cls) "type"))
   (Ψ⊢T<:T (base-Ψ) (exact l_cls) (observable-T T))
   ------------------------------------------------
   (Σ⊢v:T Σ (ref l_obj) T)]

  [(where (obj "type" g ρ) (lookup-Σ Σ l_obj))
   (Ψ⊢T<:T (base-Ψ) (Type (subof l_obj)) (observable-T T))
   -------------------------------------------------------
   (Σ⊢v:T Σ (ref l_obj) T)])

(define-judgment-form SP-conjecture
  #:mode (⊢object:T I I)
  #:contract (⊢object:T object T)

  [(where #f (eq))
   (Ψ⊢T<:T (base-Ψ) (exact l) (observable-T T))
   -----------------------------
   (⊢object:T (obj l g ρ) T)])
(define-metafunction SP-conjecture
  observable-T : T -> T
  [(observable-T (-> (T_arg ...) T_ret)) dynamic]
  [(observable-T T) T])

(define-metafunction SP-conjecture
  compile-implies-terminate-implies-well-typed-e : e+ -> boolean
  [(compile-implies-terminate-implies-well-typed-e e+)
   (terminate-implies-well-typed-e e- T)
   (judgment-holds (ΨΓΓ⊢e+↝e-:T (base-Ψ)
                                (base-Γ)
                                (base-Γ)
                                e+
                                e-
                                T))]
  [(compile-implies-terminate-implies-well-typed-e e+)
   #t])
(define-metafunction SP-conjecture
  terminate-implies-well-typed-e : e- T -> boolean
  [(terminate-implies-well-typed-e e- T)
   (Σ⊢v:T Σ v T)
   (judgment-holds (-->ⁿ (load [() () (expr e-)])
                         10000
                         [Σ l (expr v)]))]
  [(terminate-implies-well-typed-e e- T)
   #t])
;; safety of e+
(redex-check SP-conjecture
             e+
             (term (compile-implies-terminate-implies-well-typed-e e+))
             #:attempts 10000)

(define terminate-counter (box 0))
(define nonterminate-counter (box 0))
(define (inc ctr)
  (set-box! ctr (add1 (unbox ctr))))

(define-metafunction SP-conjecture
  well-typed-implies-only-stuck-at-good-state : program+ -> boolean
  [(well-typed-implies-only-stuck-at-good-state program+)
   (only-stuck-at-good-state program-)
   (judgment-holds (⊢program+↝program- program+ program-))]
  [(well-typed-implies-only-stuck-at-good-state program+)
   #t])
(define-metafunction SP-conjecture
  only-stuck-at-good-state : program- -> boolean
  [(only-stuck-at-good-state program-)
   ,(begin (inc terminate-counter) #t)
   (judgment-holds (-->ⁿ (load program-) 1000 p))
   (where #t (terminate-or-error p))]
  [(only-stuck-at-good-state program-)
   ,(begin (inc nonterminate-counter) #t)
   (judgment-holds (-->ⁿ (load program-) 1000 p))
   (where #t (reducible p))]
  [(only-stuck-at-good-state program-)
   #f])
;; safety of program+
(redex-check SP-conjecture
             program+
             (term (well-typed-implies-only-stuck-at-good-state program+))
             #:attempts 100000)
(printf "found ~a terminating programs\n" (unbox terminate-counter))
(printf "found ~a nonterminating programs\n" (unbox nonterminate-counter))
