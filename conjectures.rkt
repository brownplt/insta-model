#lang racket
(require redex)
(require "utilities.rkt")
(require "desugar.rkt")
(require "compile.rkt")
(require "runtime.rkt")

(random-seed 2022)

(define-extended-language SP-conjecture
  SP-dynamics)

(define (inc ctr)
  (set-box! ctr (add1 (unbox ctr))))

(define (reduce-to-v gas state)
  (cond
    [(or (zero? gas) (redex-match? SP-conjecture [Σ l (expr v)] state))
     state]
    [else
     (match (apply-reduction-relation red-p state)
       [(list next-state)
        (reduce-to-v (sub1 gas) next-state)]
       [(list)
        state])]))

(define (reduce gas state)
  (cond
    [(zero? gas) state]
    [else
     (match (apply-reduction-relation red-p state)
       [(list next-state)
        (reduce (sub1 gas) next-state)]
       [(list)
        state]
       [otherwise
        (println state)
        (println (apply-reduction-relation/tag-with-names red-p state))
        (raise "???")])]))

;; transition the input state until it gets stuck or we reach `number` steps.
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


(define expr-well-typed-counter (box 0))
(define expr-value-counter (box 0))
(define expr-error-counter (box 0))
(define expr-nonterminate-counter (box 0))
(define-metafunction SP-conjecture
  redex-maybe-compile-expr : e+ -> any
  [(redex-maybe-compile-expr e+)
   [e- T]
   (judgment-holds (ΨΓΓ⊢e+↝e-:T (base-Ψ)
                                (base-Γ)
                                (base-Γ)
                                e+
                                e-
                                T))]
  [(redex-maybe-compile-expr e+)
   #f])
(define (maybe-compile-expr e+)
  (term (redex-maybe-compile-expr ,e+)))

(define-metafunction SP-conjecture
  redex-maybe-get-Σv : p -> any
  [(redex-maybe-get-Σv [Σ l (expr v)])
   [Σ v]]
  [(redex-maybe-get-Σv p)
   #f])
(define (maybe-get-Σv p)
  (term (redex-maybe-get-Σv ,p)))
(define (racket-compile-implies-terminate-implies-well-typed-e e+)
  (let* ([e-T (maybe-compile-expr e+)])
    (implies e-T
             (match-let ([(list e- T) e-T])
               (inc expr-well-typed-counter)
               (let* ([p (reduce-to-v 100 (term (load [() () (expr ,e-)])))])
                 (cond
                   [(redex-match? SP-dynamics (error any) p)
                    ;; terminate as error
                    (begin
                      (inc expr-error-counter)
                      #t)]
                   [else
                    (let* ([Σv? (maybe-get-Σv p)])
                      (if Σv?
                          ;; terminate as a value
                          (begin
                            (inc expr-value-counter)
                            (match-let ([(list Σ v) Σv?])
                              (term (Σ⊢v:T ,Σ ,v ,T))))
                          ;; other cases
                          (begin
                            (inc expr-nonterminate-counter)
                            (term (show ,p))
                            #t)))]))))))
(define-metafunction SP-conjecture
  compile-implies-terminate-implies-well-typed-e : e+ -> boolean
  [(compile-implies-terminate-implies-well-typed-e e+)
   ,(racket-compile-implies-terminate-implies-well-typed-e (term e+))])
;; safety of e+
(redex-check SP-conjecture
             e+
             (term (compile-implies-terminate-implies-well-typed-e e+))
             #:attempts 10000)
(define expr-well-typed (unbox expr-well-typed-counter))
(define expr-terminate-to-value (unbox expr-value-counter))
(define expr-terminate-to-error (unbox expr-error-counter))
(define expr-nonterminate (unbox expr-nonterminate-counter))
(when (not (= (+ expr-terminate-to-error expr-terminate-to-value expr-nonterminate)
                 expr-well-typed))
  (raise (format "testing expressions. ~a ≠ ~a + ~a + ~a"
                 expr-well-typed
                 expr-terminate-to-value
                 expr-terminate-to-error
                 expr-nonterminate)))
(printf "found ~a well-typed expressions.\n" expr-well-typed)
(printf "~a of them reduce to a value of the expected type.\n" expr-terminate-to-value)
(printf "~a of them reduce to an error.\n" expr-terminate-to-error)
(printf "~a of them don't reduce to a value within the step limit.\n" expr-nonterminate)

(define prog-well-typed-counter (box 0))
(define prog-terminate-counter (box 0))
(define prog-nonterminate-counter (box 0))
(define prog-else-counter (box 0))

(define-metafunction SP-conjecture
  redex-maybe-compile : program+ -> any
  [(redex-maybe-compile program+)
   program-
   (judgment-holds (⊢program+↝program- program+ program-))]
  [(redex-maybe-compile program+) #f])
(define (maybe-compile program+)
  (term (redex-maybe-compile ,program+)))

(define-metafunction SP-conjecture
  redex-maybe-reduce : program- -> any
  [(redex-maybe-reduce program-)
   p
   (judgment-holds (-->ⁿ (load program-) 50 p))]
  [(redex-maybe-reduce program-)
   #f])
(define (maybe-reduce program-)
  (term (redex-maybe-reduce ,program-)))

(define-metafunction SP-conjecture
  well-typed-implies-only-stuck-at-good-state : program+ -> boolean
  [(well-typed-implies-only-stuck-at-good-state program+)
   ,(racket-well-typed-implies-only-stuck-at-good-state (term program+))])
(define (racket-well-typed-implies-only-stuck-at-good-state program+)
  (let* ([program-? (maybe-compile program+)])
    ;; compile iff well-typed
    ;; We test whether well-typed-ness implies safety
    (implies program-?
             (begin
               (inc prog-well-typed-counter)
               (let* ([p (maybe-reduce program-?)])
                 (if (term (terminate-or-error ,p))
                     (begin
                       (inc prog-terminate-counter)
                       #t)
                     (begin
                       (inc prog-nonterminate-counter)
                       (term (reducible ,p)))))))))
;; You can pick a number larger than 15000 but it will be much slower.
;; Testing 25000 programs takes 1h on Kuang-Chen's laptop.
(define program-total 15000)
;; safety of program+
(redex-check SP-conjecture
             program+
             (term (well-typed-implies-only-stuck-at-good-state program+))
             #:attempts program-total)
(define program-well-typed (unbox prog-well-typed-counter))
(define program-terminate (unbox prog-terminate-counter))
(define program-nonterminate (unbox prog-nonterminate-counter))
(when (not (= (+ program-terminate program-nonterminate) program-well-typed))
  (println (unbox prog-else-counter))
  (raise (format "~a ≠ ~a + ~a"
                 program-well-typed
                 program-terminate
                 program-nonterminate)))
(printf "found ~a well-typed programs.\n" program-well-typed)
(printf "~a of them terminate.\n" program-terminate)
(printf "~a of them don't terminate within the step limit.\n" program-nonterminate)
