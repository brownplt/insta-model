#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "statics.rkt")
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-statics
  ;; environments maps cid to runtime class description
  ;; environments maps variables to values
  (ρ ([x v] ...))
  ;; heaps map addresses (heap labels) to values
  (Σ ([number h] ...))
  ;; heap labels
  (l number
     ;; special heap addresses reserved by constants
     (con c)
     ;; other primitive values, e.g. builtin methods
     string)
  ;; heap values
  (h ;; According to Python's Data model: "every object has an identity, a type and a value"
   ;; We don't need to store the identity here -- the heap Σ already did it.
   ;; But we still need a type (l), and an object value (g).
   (l g))
  ;; "Values" in the sense of Python's Data model. They are *before* heap values (h), hence g
  (g c
     (tuple-syntax v ...)
     (set-syntax v ...)
     (dict-syntax (v v) ...)
     (function ρ ([x T] ...) T s ...)
     (class (l ...) ([string l] ...))
     (prim-op string))
  ;; values are just heap addresses
  (v (ref l))
  ;; at runtime immediate values can go into expressions.
  (e .... v)
  ;; expression contexts
  (E hole
     (tuple-syntax v ... E e ...)
     (set-syntax v ... E e ...)
     (dict-syntax (v v) ... (E e) (e e) ...)
     (dict-syntax (v v) ... (v E) (e e) ...)
     (attribute E string)
     (is E e)
     (is v E)
     (is-not E e)
     (is-not v E)
     (if E e e)
     (v ... E e ...)
     (reveal-type any ... E))
  ;; statement contexts
  (S (return E)
     (claim x t)
     (define/assign x t E)
     (if E (s ...) (s ...))
     (expr E))
  )

(define-metafunction SP-dynamics
  alloc : Σ h -> (Σ l)
  [(alloc Σ h)
   ((extend Σ [l h]) l)
   (where l ,(length (term Σ)))])

(module+ test
  (test-equal (term (lookup-Σ (base-Σ) (con 2)))
              (term ("int" 2)))
  (test-equal (term (lookup-Σ (base-Σ) (con 2.0)))
              (term ("float" 2.0)))
  (test-equal (term (lookup-Σ (base-Σ) (con #t)))
              (term ("bool" #t)))
  (test-equal (term (lookup-Σ (base-Σ) (con #f)))
              (term ("bool" #f))))
(define-metafunction SP-dynamics
  lookup-Σ : Σ l -> h
  [(lookup-Σ Σ number) (lookup Σ number)]
  [(lookup-Σ Σ (con integer))
   ("int" integer)]
  [(lookup-Σ Σ (con number))
   ("float" number)]
  [(lookup-Σ Σ (con boolean))
   ("bool" boolean)]
  [(lookup-Σ Σ (con string))
   ("str" string)]
  [(lookup-Σ Σ (con None))
   ("NoneType" None)]
  [(lookup-Σ Σ "float")
   ("type" (class ("object")
             (["__init__" "float.__init__"]
              ["__gt__" "float.__gt__"]
              ["__eq__" "float.__eq__"]
              ["__neq__" "float.__neq__"]
              ["__add__" "float.__add__"])))]
  [(lookup-Σ Σ "int")
   ("type" (class ("float")
             (["__init__" "int.__init__"])))]
  [(lookup-Σ Σ "bool")
   ("type" (class ("int")
             (["__init__" "bool.__init__"])))]
  [(lookup-Σ Σ "str")
   ("type" (class ("object")
             (["__init__" "str.__init__"])))]
  [(lookup-Σ Σ "tuple")
   ("type" (class ("object") (["__init__" "tuple.__init__"])))]
  [(lookup-Σ Σ "set")
   ("type" (class ("object") (["__init__" "set.__init__"])))]
  [(lookup-Σ Σ "dict")
   ("type" (class ("object") (["__init__" "dict.__init__"])))]
  [(lookup-Σ Σ "object")
   ("type" (class () (["__init__" "object.__init__"])))]
  [(lookup-Σ Σ "type")
   ("type" (class ("object") ()))])

(define-metafunction SP-dynamics
  base-Σ : -> Σ
  [(base-Σ) ()])

(define-metafunction SP-dynamics
  base-ρ : -> ρ
  [(base-ρ)
   ([object (ref "object")]
    [float (ref "float")]
    [int (ref "int")]
    [bool (ref "bool")]
    [str (ref "str")]
    [dict (ref "dict")]
    [set (ref "set")]
    [type (ref "type")])])

(module+ test
  (test-equal (term #t) (term (falsy ("NoneType" None))))
  (test-equal (term #t) (term (falsy ("float" 0.0))))
  (test-equal (term #t) (term (falsy ("int" 0))))
  (test-equal (term #t) (term (falsy ("bool" #f))))
  (test-equal (term #t) (term (falsy ("tuple" (tuple-syntax)))))
  (test-equal (term #t) (term (falsy ("set" (set-syntax)))))
  (test-equal (term #t) (term (falsy ("dict" (dict-syntax)))))
  (test-equal (term #f) (term (falsy ("int" 2))))
  (test-equal (term #f) (term (falsy ("bool" #t))))
  (test-equal (term #f) (term (falsy ("tuple" (tuple-syntax (ref (con None))))))))
(define-metafunction SP-dynamics
  falsy : h -> boolean
  [(falsy ("NoneType" None)) #t]
  [(falsy ("float" 0.0)) #t]
  [(falsy ("int" 0)) #t]
  [(falsy ("bool" #f)) #t]
  [(falsy ("tuple" (tuple-syntax))) #t]
  [(falsy ("set" (set-syntax))) #t]
  [(falsy ("dict" (dict-syntax))) #t]
  [(falsy h) #f])

;; TODO this needs a major revision
(module+ test
  (test-equal (term (get-attr (base-Σ) (ref "float") "__add__"))
              (term ((base-Σ) (ref "float.__add__"))))
  (test-equal (term (get-attr (base-Σ) (ref "bool") "__add__"))
              (term ((base-Σ) (ref "float.__add__")))))
(define-metafunction SP-dynamics
  get-attr : Σ v string -> (Σ v)
  [(get-attr Σ (ref l) string_key)
   (Σ (ref l_val))
   (where ("type" (class (l_par ...)
                    ([string_1 l_1] ...
                     [string_key l_val]
                     [string_2 l_2] ...)))
          (lookup-Σ Σ l))]
  [(get-attr Σ (ref l_slf) string_key)
   (get-attr Σ (ref l_par) string_key)
   (where ("type" (class (l_par)
                    ([string l] ...)))
          (lookup-Σ Σ l_slf))])

(module+ test
  (test-->> e→e
            (term ((base-Σ) (base-ρ) 2))
            (term ((base-Σ) (base-ρ) (ref (con 2)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) int))
            (term ((base-Σ) (base-ρ) (ref "int"))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (tuple-syntax 2 "foo")))
            (term ((extend (base-Σ)
                           [0 ("tuple" (tuple-syntax (ref (con 2))
                                                     (ref (con "foo"))))])
                   (base-ρ)
                   (ref 0))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (set-syntax 2 "foo")))
            (term ((extend (base-Σ)
                           [0 ("set" (set-syntax (ref (con 2))
                                                 (ref (con "foo"))))])
                   (base-ρ)
                   (ref 0))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (dict-syntax [2 "foo"] ["bar" 3])))
            (term ((extend (base-Σ)
                           [0 ("dict"
                               (dict-syntax [(ref (con 2)) (ref (con "foo"))]
                                            [(ref (con "bar")) (ref (con 3))]))])
                   (base-ρ) (ref 0))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (is int int)))
            (term ((base-Σ) (base-ρ) (ref (con #t)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (is int float)))
            (term ((base-Σ) (base-ρ) (ref (con #f)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (is-not int int)))
            (term ((base-Σ) (base-ρ) (ref (con #f)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (is-not int float)))
            (term ((base-Σ) (base-ρ) (ref (con #t)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (if 1 2 3)))
            (term ((base-Σ) (base-ρ) (ref (con 2)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (if 0 2 3)))
            (term ((base-Σ) (base-ρ) (ref (con 3)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (if None 2 3)))
            (term ((base-Σ) (base-ρ) (ref (con 3)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (if #t 2 3)))
            (term ((base-Σ) (base-ρ) (ref (con 2)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (if #f 2 3)))
            (term ((base-Σ) (base-ρ) (ref (con 3)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (attribute int "__add__")))
            (term ((base-Σ) (base-ρ) (ref "float.__add__"))))
  #; ;; TODO
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (int "123")))
            (term ((base-Σ) (base-ρ) (ref (con 123)))))
  #; ;; TODO
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (str 1.0)))
            (term ((base-Σ) (base-ρ) (ref (con "1.0"))))))
  
(define e→e
  (reduction-relation
   SP-dynamics
   #:domain (Σ ρ e)
   (--> (in-hole (Σ ρ E) c)
        (in-hole (Σ ρ E) (ref (con c)))
        "constant")
   (--> (in-hole (Σ ρ E) x)
        (in-hole (Σ ρ E) (lookup ρ x))
        "lookup")
   (--> (in-hole (Σ_1 ρ E) (tuple-syntax v ...))
        (in-hole (Σ_2 ρ E) (ref l))
        (where (Σ_2 l) (alloc Σ_1 ("tuple" (tuple-syntax v ...))))
        "tuple")
   (--> (in-hole (Σ_1 ρ E) (set-syntax v ...))
        (in-hole (Σ_2 ρ E) (ref l))
        (where (Σ_2 l) (alloc Σ_1 ("set" (set-syntax v ...))))
        "set")
   (--> (in-hole (Σ_1 ρ E) (dict-syntax [v_key v_val] ...))
        (in-hole (Σ_2 ρ E) (ref l))
        (where (Σ_2 l) (alloc Σ_1 ("dict" (dict-syntax [v_key v_val] ...))))
        "dict")
   (--> (in-hole (Σ ρ E) (is v_1 v_2))
        (in-hole (Σ ρ E) (= v_1 v_2))
        "is")
   (--> (in-hole (Σ ρ E) (is-not v_1 v_2))
        (in-hole (Σ ρ E) (≠ v_1 v_2))
        "is-not")
   (--> (in-hole (Σ ρ E) (if (ref l) e_thn e_els))
        (in-hole (Σ ρ E) e_thn)
        (where h (lookup-Σ Σ l))
        (where #f (falsy h))
        "if truthy")
   (--> (in-hole (Σ ρ E) (if (ref l) e_thn e_els))
        (in-hole (Σ ρ E) e_els)
        (where h (lookup-Σ Σ l))
        (where #t (falsy h))
        "if falsy")
   (--> (in-hole (Σ_1 ρ E) (attribute v_map string))
        (in-hole (Σ_2 ρ E) v_val)
        (where (Σ_2 v_val) (get-attr Σ_1 v_map string))
        "attribute")
   #; ;; TODO
   (--> (in-hole (Σ_1 ρ E) ((ref l_prc) v_arg ...))
        (in-hole (Σ_2 ρ E) e_ret)
        (where h_prc (lookup Σ_1 l_prc))
        (where (Σ_2 e_ret) (call Σ_1 v_prc v_arg ...))
        "procedure call (builtin heap value)")
   ))
