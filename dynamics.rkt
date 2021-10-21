#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "statics.rkt")
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-statics
  ;; primitive values
  (c ....
     (prim-method string v_self))
  ;; (all) values
  (v c
     (address l))
  ;; heaps
  (Σ ((l h+☠) ...))
  ;; heap labels
  (l number)
  ;; heap values
  (h v
     (x_class (v_slot ...) ([string_dict v_dict] ...))
     (function ([x_arg t_arg] ...) t_ret s_body ...))
  ;; heap values or uninitialized memory
  (h+☠ h ☠)
  ;; values may go into expressions
  (e ....
     v (var l)
     ;; introduced by funcion calls
     (local s ...))
  ;; program execution states
  (p (Ψ Σ (s ...)))
  ;; program execution states with hole
  (P (Ψ Σ (S s ...)))
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
     (local S s ...)
     hole)
  )

(define-metafunction SP-dynamics
  alloc-one : Σ h+☠ -> (Σ l)
  [(alloc-one Σ_1 h+☠)
   (Σ_2 l)
   (where ([l_used h+☠_used] ...) Σ_1)
   (where l ,(for/first ([i (in-naturals)]
                          #:unless (set-member? (list->set (term (l_used ...))) i))
               i))
   (where Σ_2 (extend Σ_1 [l h+☠]))])

(define-metafunction SP-dynamics
  alloc : Σ h+☠ ... -> (Σ l ...)
  [(alloc Σ) (Σ)]
  [(alloc Σ_1 h+☠_1 h+☠_2 ...)
   (Σ_3 l_1 l_2 ...)
   (where (Σ_2 l_1) (alloc-one Σ_1 h+☠_1))
   (where (Σ_3 l_2 ...) (alloc Σ_2 h+☠_2 ...))])

(define-metafunction SP-dynamics
  update : Σ l h -> Σ
  [(update (any_1 ... [l h+☠_old] any_2 ...) l h_new)
   (any_1 ... [l h_new] any_2 ...)])

(require redex/tut-subst)
(define-metafunction SP-dynamics
  subst : (x ...) (any_v ...) any_term -> any
  [(subst (x ...) (any_v ...) any_term)
   ,(subst/proc x? (term (x ...)) (term (any_v ...)) (term any_term))])
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
   ;; expressions
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
   (--> (in-hole (Ψ Σ_1 (S s ...)) ((address l_fun) v_arg ...))
        (in-hole (Ψ Σ_2 (S s ...)) (local s_body2 ...))
        (judgment-holds (lookupo Σ_1 l_fun (function ([x_arg t_arg] ...) t_ret s_body1 ...)))
        (where (Σ_2 l_arg ...) (alloc Σ_1 v_arg ...))
        (where (s_body2 ...) (subst (x_arg ...) ((var l_arg) ...) (s_body1 ...)))
        "enter function")
   (--> (in-hole (Ψ Σ (S s ...)) (local (return v) s ...))
        (in-hole (Ψ Σ (S s ...)) v)
        "leave function")
   ;; statements
   (--> (in-hole (Ψ Σ_1 (hole s_1 ...)) (def x_fun ([x_arg t_arg] ...) t_ret s_body ...))
        (Ψ Σ_3 (s_2 ...))
        (where (Σ_2 l_fun) (alloc Σ_1 (function ([x_arg t_arg] ...) t_ret s_body ...)))
        (where (Σ_3 l_var) (alloc Σ_2 (address l_fun)))
        (where (s_2 ...) (subst (x_fun) ((var l_var)) (s_1 ...)))
        "def function")
   (--> (in-hole (Ψ Σ_1 (hole s_1 ...)) (claim x t))
        (Ψ Σ_2 (s_2 ...))
        (where (Σ_2 l) (alloc Σ_1 ☠))
        (where (s_2 ...) (subst (x) ((var l)) (s_1 ...)))
        "claim variable")
   (--> (in-hole (Ψ Σ_1 (hole s ...)) (define/assign (var l) v))
        (Ψ Σ_2 (s ...))
        (where Σ_2 (update Σ_1 l v))
        "define/assign")
   ;; let values go
   #;
   (--> (Ψ Σ ((expr v) s ...))
        (Ψ Σ (s ...))
        "skip values")))

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
  (test-->>
   red
   (term ((base-Ψ)
          ()
          ((def f ([x int]) int
             (return ((attribute x "__add__") 3)))
           (expr (f 2)))))
   (term ((base-Ψ)
          ((2 2)
           (1 (address 0))
           (0
            (function
             ((x int))
             int
             (return ((attribute x "__add__") 3)))))
          ((expr 5)))))
  (test-->>
   red
   (term ((import-from "__static__" ("PyDict" "CheckedDict"))
          (define/assign x (subscript CheckedDict (tuple-syntax str int))
            ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1))))
          (delete (subscript x "foo")))))
  (test-results))