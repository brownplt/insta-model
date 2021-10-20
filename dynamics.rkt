#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "statics.rkt")
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-statics
  ;; primitive values
  (u number
     boolean
     (prim-method string v_self))
  ;; (all) values
  (v (ref l)
     u)
  ;; heaps
  (Σ ((l h+☠) ...))
  ;; heap labels
  (l number)
  ;; heap values
  (h v
     (x_class (v_slot ...) ([string_dict v_dict] ...))
     number)
  ;; heap values or uninitialized memory
  (h+☠ h ☠)
  ;; values may go into expressions
  (e .... v (var l))
  ;; program execution states
  (p (Ψ Σ (s ...)))
  ;; program execution states with hole
  (P (Ψ Σ (hole s ...))
     (Ψ Σ (S s ...)))
  ;; statements with hole
  (S (return E)
     (define/assign (var l) E)
     (expr E)
     hole)
  ;; expressions with hole
  (E (dict-syntax (v v) ... (E e) (e e) ...)
     (dict-syntax (v v) ... (v E) (e e) ...)
     (subscript E e)
     (subscript v E)
     (attribute E string)
     (v ... E e ...)
     hole)
  )

(define-metafunction SP-dynamics
  alloc : Σ h+☠ -> (Σ l)
  [(alloc Σ h+☠)
   ,(let* ([used-labels (list->set (map first (term Σ)))]
           [l (for/first ([i (in-naturals)]
                          #:unless (set-member? used-labels i))
                i)]
           [Σ (term (extend Σ [,l h+☠]))])
      (term (,Σ ,l)))])

(define-metafunction SP-dynamics
  update : Σ l h -> Σ
  [(update (any_1 ... [l h+☠_old] any_2 ...) l h_new)
   (any_1 ... [l h_new] any_2 ...)])

(require redex/tut-subst)
(define-metafunction SP-dynamics
  subst : x any_v any_term -> any
  [(subst x any_v any_term)
   ,(subst/proc x? (list (term x)) (list (term any_v)) (term any_term))])
(define x? (redex-match SP-dynamics x))

(define-metafunction SP-dynamics
  delta : Σ string v ... -> (Σ v)
  [(delta Σ "int.__add__" number_1 number_2)
   (Σ ,(+ (term number_1) (term number_2)))]
  [(delta Σ "int.__add__" number_1 boolean_2)
   (Σ ,(+ (term number_1) (if (term boolean_2) 1 0)))])

(define red
  (reduction-relation
   SP-dynamics
   #:domain (Ψ Σ (s ...))
   (--> (in-hole (Ψ Σ (S s ...)) (var l))
        (in-hole (Ψ Σ (S s ...)) (lookup Σ l))
        "lookup variable")
   (--> (in-hole (Ψ Σ (S s ...)) (attribute number "__add__"))
        (in-hole (Ψ Σ (S s ...)) (prim-method "int.__add__" number))
        "access attribute")
   (--> (in-hole (Ψ Σ_1 (S s ...))
                 ((prim-method string v_self) v_arg ...))
        (in-hole (Ψ Σ_2 (S s ...))
                 v_ret)
        (where (Σ_2 v_ret) (delta Σ_1 string v_self v_arg ...))
        "delta")
   (--> (in-hole (Ψ Σ_1 (hole s_1 ...)) (claim x t))
        (Ψ Σ_2 (s_2 ...))
        (where (Σ_2 l) (alloc Σ_1 ☠))
        (where (s_2 ...) (subst x (var l) (s_1 ...)))
        "claim variable")
   (--> (in-hole (Ψ Σ_1 (hole s ...)) (define/assign (var l) v))
        (Ψ Σ_2 (s ...))
        (where Σ_2 (update Σ_1 l v))
        "define/assign")))

(module+ test
  (test-->>
   red
   (term ((base-Ψ)
          ()
          ((expr 42))))
   (term ((base-Ψ)
          ()
          ((expr 42)))))
  (test-->>
   red
   (term ((base-Ψ)
          ()
          ((claim x int)
           (define/assign x 2)
           (expr x))))
   (term ((base-Ψ)
          ((0 2))
          ((expr 2)))))
  (test-->>
   red
   (term ((base-Ψ)
          ()
          ((claim x int)
           (claim y int)
           (define/assign x 2)
           (define/assign y 3)
           (expr ((attribute x "__add__") y)))))
   (term ((base-Ψ)
          ((1 3)
           (0 2))
          ((expr 5)))))
  (test-->>
   red
   (term ((base-Ψ)
          ()
          ((claim x int)
           (claim y int)
           (define/assign x 2)
           (define/assign y #t)
           (expr ((attribute x "__add__") y)))))
   (term ((base-Ψ)
          ((1 #t)
           (0 2))
          ((expr 3)))))
  (test-results))