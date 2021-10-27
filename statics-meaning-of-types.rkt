#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "statics-basic-definitions.rkt")
(require "statics-utilities.rkt")
(provide (all-defined-out))

(define (first-free-cid Ψ)
  (let ([used-cids (for/set ([entry Ψ]
                             #:when (number? (car entry)))
                     (car entry))])
    (for/first ([i (in-naturals)]
                #:unless (set-member? used-cids i))
      i)))

(module+ test
  (test-equal (term (Ψ-alloc (base-Ψ) (class C "object" () ())))
              (term ((extend (base-Ψ) [0 (class C "object" () ())])
                     0))))
(define-metafunction SP-statics
  Ψ-alloc : Ψ C+☠ -> (Ψ cid)
  [(Ψ-alloc Ψ_1 C+☠)
   (Ψ_2 number)
   (where number ,(first-free-cid (term Ψ_1)))
   (where Ψ_2 (extend Ψ_1 [number C+☠]))])

(define-metafunction SP-statics
  Ψ-init : Ψ cid C -> Ψ
  [(Ψ-init (any_1 ... [cid ☠] any_2 ...) cid C)
   (any_1 ... [cid C] any_2 ...)])

(define-metafunction SP-statics
  base-Ψ : -> Ψ
  [(base-Ψ) ()])

(define-metafunction SP-statics
  base-Γ : -> Γ
  [(base-Γ)
   ([object (classitself "object")]
    [float (classitself "float")]
    [int (classitself "int")]
    [bool (classitself "bool")]
    [str (classitself "str")]
    [dict (classitself "dict")]
    [type (classitself "type")])])

(define-judgment-form SP-statics
  #:mode (evalo** I I I O)
  #:contract (evalo** Ψ Γ ((t ...) ...) ((T ...) ...))
  [(evalo* Ψ Γ (t ...) (T ...)) ...
   -------------------
   (evalo** Ψ Γ ((t ...) ...) ((T ...) ...))])

(define-judgment-form SP-statics
  #:mode (evalo* I I I O)
  #:contract (evalo* Ψ Γ (t ...) (T ...))
  [(evalo Ψ Γ t T) ...
   -------------------
   (evalo* Ψ Γ (t ...) (T ...))])

(module+ test
  (check-judgment-holds*
   (evalo (base-Ψ) (base-Γ) "int" (instancesof "int"))
   (evalo (base-Ψ) (extend (base-Γ) [CheckedDict (prim-generic "CheckedDict")])
          (subscript CheckedDict (tuple-syntax str int))
          (instancesof ("CheckedDict" "str" "int")))))
(define-judgment-form SP-statics
  #:mode (evalo I I I O)
  #:contract (evalo Ψ Γ t T)

  ;; Compute type values from type expressions

  [----
   (evalo Ψ Γ dynamic dynamic)]

  [------------------------- "None"
   (evalo Ψ Γ None (instancesof "None"))]

  [(lookupo Γ x (prim-generic "Callable"))
   (evalo Ψ Γ t_arg T_arg) ...
   (evalo Ψ Γ t_ret T_ret)
   ------------------------- "Callable"
   (evalo Ψ Γ
          (subscript x (tuple-syntax t_arg ... t_ret))
          (-> ([☠ T_arg] ...) T_ret))]

  [(lookupo Γ x (prim-generic "CheckedDict"))
   (evalo Ψ Γ t_key (instancesof cid_key))
   (evalo Ψ Γ t_val (instancesof cid_val))
   ------------------------- "CheckedDict"
   (evalo Ψ Γ 
          (subscript x (tuple-syntax t_key t_val))
          (instancesof ("CheckedDict" cid_key cid_val)))]

  ;; TODO this doesn't look right. We should do something
  [(lookupo Γ x (prim-generic "Final"))
   (evalo Ψ Γ t T)
   ------------------------- "Final"
   (evalo Ψ Γ (subscript x t) T)]

  [(lookupo Γ x (prim-generic "Optional"))
   (evalo Ψ Γ t (instancesof cid))
   ------------------------- "Optional"
   (evalo Ψ Γ (subscript x t) ("optional" cid))]

  [(evalo Ψ Γ ,(string->symbol (term string)) T)
   ------------------------- "Lookup string"
   (evalo Ψ Γ string T)]

  [(lookupo Γ x (classitself cid))
   ------------------------- "Lookup class"
   (evalo Ψ Γ x (instancesof cid))]

  [(lookupo Γ x dynamic)
   ------------------------- "Lookup dynamic"
   (evalo Ψ Γ x dynamic)])

(module+ test
  (check-judgment-holds*
   (Ψ⊢cid<:cid () "int" "float")
   (Ψ⊢cid<:cid () "bool" "float")
   (Ψ⊢cid<:cid () "None" "object")
   (Ψ⊢cid<:cid () "None" "None"))
  (check-not-judgment-holds*
   (Ψ⊢cid<:cid () "float" "int")
   (Ψ⊢cid<:cid () "float" "bool")
   (Ψ⊢cid<:cid () "object" "None")))
(define-judgment-form SP-statics
  #:mode (Ψ⊢cid<:cid I I I)
  #:contract (Ψ⊢cid<:cid Ψ cid cid)
  ;; subclass relation

  [------------------------- "object-is-the-top"
   (Ψ⊢cid<:cid Ψ cid "object")]

  [------------------------- "refl"
   (Ψ⊢cid<:cid Ψ cid cid)]

  [(where (class any_desc cid_par any_spec ...) (lookup-class Ψ cid_slf))
   (Ψ⊢cid<:cid Ψ cid_par cid_bnd)
   ------------------------ "super"
   (Ψ⊢cid<:cid Ψ cid_slf cid_bnd)]
  )

(module+ test
  (check-judgment-holds*
   (Ψ⊢T≲T (base-Ψ) (instancesof "int") (instancesof "float"))
   (Ψ⊢T≲T (base-Ψ) (instancesof "bool") (instancesof "int"))
   (Ψ⊢T≲T (base-Ψ) (instancesof "bool") (instancesof "object"))
   (Ψ⊢T≲T (base-Ψ) (instancesof "str") dynamic)
   (Ψ⊢T≲T (base-Ψ) dynamic (instancesof "str"))
   (Ψ⊢T≲T (base-Ψ) (prim-generic "CheckedDict") dynamic)
   (Ψ⊢T≲T (base-Ψ) (-> () (instancesof "int")) (-> () (instancesof "float")))
   ;; optional types
   (Ψ⊢T≲T (base-Ψ) (instancesof "int") ("optional" "int"))
   (Ψ⊢T≲T (base-Ψ) (instancesof "None") ("optional" "int"))
   (Ψ⊢T≲T (base-Ψ) ("optional" "int") ("optional" "float"))
   (Ψ⊢T≲T (base-Ψ) ("optional" "int") (instancesof "object")))
  (check-not-judgment-holds*
   (Ψ⊢T≲T (base-Ψ) (-> () (instancesof "str")) (-> () (instancesof "int")))
   (Ψ⊢T≲T (base-Ψ) (-> () (instancesof "float")) (-> () (instancesof "int")))
   (Ψ⊢T≲T (base-Ψ) (-> ([x (instancesof "int")]) dynamic) (-> ([x (instancesof "float")]) dynamic))))
(define-judgment-form SP-statics
  #:mode (Ψ⊢T≲T I I I)
  #:contract (Ψ⊢T≲T Ψ T T)
  ;; Is it sensible to use a value of class C_0 as as a value of
  ;; class C_1?
  ;; (a.k.a. consistent subtyping)

  [------------------------ "dynamic-L"
   ;; TODO: I think T must be of form (instancesof cid)
   (Ψ⊢T≲T Ψ dynamic T)]

  [------------------------ "dynamic-R"
   (Ψ⊢T≲T Ψ T dynamic)]

  [(side-condition
    (= (len (T_larg ...))
       (len (T_rarg ...))))
   (Ψ⊢T≲T Ψ T_rarg T_larg) ...
   (Ψ⊢T≲T Ψ T_lret T_rret)
   ------------------------ "callable"
   (Ψ⊢T≲T Ψ
          (-> ([x+☠ T_larg] ...) T_lret)
          (-> ([x+☠ T_rarg] ...) T_rret))]

  [------------------------ "refl"
   (Ψ⊢T≲T Ψ T T)]

  [(Ψ⊢cid<:cid Ψ cid_1 cid_2)
   ------------------------ "super"
   (Ψ⊢T≲T Ψ (instancesof cid_1) (instancesof cid_2))]
 
  [(Ψ⊢T≲T Ψ T (instancesof cid))
   ------------------------ "Optional[cid]-R1"
   (Ψ⊢T≲T Ψ T ("optional" cid))]
  
  [------------------------ "Optional[cid]-R2"
   (Ψ⊢T≲T Ψ (instancesof "None") ("optional" cid))]

  [(Ψ⊢T≲T Ψ (instancesof "None") T)
   (Ψ⊢T≲T Ψ (instancesof cid) T)
   ------------------------ "Optional[cid]-L"
   (Ψ⊢T≲T Ψ ("optional" cid) T)]
  )

(module+ test
  (test-equal (term (union (base-Ψ) (instancesof "int") (instancesof "int")))
              (term (instancesof "int")))
  (test-equal (term (union (base-Ψ) (instancesof "int") dynamic))
              (term dynamic))
  (test-equal (term (union (base-Ψ) dynamic (instancesof "int")))
              (term dynamic))
  (test-equal (term (union (base-Ψ) (instancesof "float") (instancesof "int")))
              (term (instancesof "float")))
  (test-equal (term (union (base-Ψ) (instancesof "int") (instancesof "float")))
              (term (instancesof "float")))
  (test-equal (term (union (base-Ψ) (instancesof "int") (instancesof "str")))
              (term dynamic))
  (test-equal (term (union (base-Ψ) (instancesof "int") (instancesof "None")))
              (term ("optional" "int")))
  (test-equal (term (union (base-Ψ) (instancesof "None") (instancesof "int")))
              (term ("optional" "int")))
  (test-equal (term (union (base-Ψ) (instancesof "None") ("optional" "int")))
              (term ("optional" "int")))
  (test-equal (term (union (base-Ψ) ("optional" "int") (instancesof "None")))
              (term ("optional" "int")))
  (test-equal (term (union (base-Ψ) ("optional" "int") ("optional" "float")))
              (term ("optional" "float"))))
(define-metafunction SP-statics
  union : Ψ T T -> T
  [(union Ψ T T) T]
  [(union Ψ dynamic T) dynamic]
  [(union Ψ T dynamic) dynamic]
  [(union Ψ (instancesof cid_1) (instancesof cid_2))
   (instancesof cid_2)
   (judgment-holds (Ψ⊢cid<:cid Ψ cid_1 cid_2))]
  [(union Ψ (instancesof cid_1) (instancesof cid_2))
   (instancesof cid_1)
   (judgment-holds (Ψ⊢cid<:cid Ψ cid_2 cid_1))]
  [(union Ψ (instancesof cid) (instancesof "None"))
   ("optional" cid)]
  [(union Ψ (instancesof "None") (instancesof cid))
   ("optional" cid)]
  [(union Ψ (instancesof cid_1) (instancesof cid_2)) dynamic]
  [(union Ψ ("optional" cid) (instancesof "None")) ("optional" cid)]
  [(union Ψ (instancesof "None") ("optional" cid)) ("optional" cid)]
  [(union Ψ ("optional" cid_1) ("optional" cid_2))
   (union Ψ (instancesof "None") (union Ψ (instancesof cid_1) (instancesof cid_2)))]
  [(union Ψ T_1 T_2) dynamic])

(define-metafunction SP-statics
  remove-None : T -> T
  [(remove-None ("optional" cid)) (instancesof cid)]
  [(remove-None (instancesof "None")) dynamic]  ;; If we want to be pedantic, should be bottom
  [(remove-None (instancesof cid)) (instancesof cid)]
  [(remove-None T) T])

(module+ test
  (test-match SP-statics C (term (lookup-class () "object")))
  (test-match SP-statics C (term (lookup-class () "type")))
  (test-match SP-statics C (term (lookup-class () "float")))
  (test-match SP-statics C (term (lookup-class () "int")))
  (test-match SP-statics C (term (lookup-class () "bool")))
  (test-match SP-statics C (term (lookup-class () "str")))
  (test-match SP-statics C (term (lookup-class () "None")))
  (test-match SP-statics C (term (lookup-class () "dict")))
  (test-match SP-statics C (term (lookup-class () "set")))
  (test-match SP-statics C (term (lookup-class () ("CheckedDict" "str" "int")))))
(define-metafunction SP-statics
  lookup-class : Ψ cid -> C
  ;; Find the meaning of a class by its cid
  ;; primitive/builtin classes are handled here
  [(lookup-class Ψ "object")
   (class object
     ☠
     ()
     (("__init__" () (instancesof "None"))))]
  [(lookup-class Ψ "type")
   (class type "object" () ())]
  [(lookup-class Ψ "float")
   (class float
     "object"
     ()
     (["__init__"
       ([☠ dynamic])
       dynamic]
      ["__neg__"
       ()
       (instancesof "float")]
      ["__add__"
       ([☠ (instancesof "float")])
       (instancesof "float")]))]
  [(lookup-class Ψ "int")
   (class int "float" () ())]
  [(lookup-class Ψ "bool")
   (class bool "int" () ())]
  [(lookup-class Ψ "str")
   (class str "object" () (("__init__" ([☠ dynamic]) dynamic)))]
  [(lookup-class Ψ "dict")
   (class dict "object"
     ()
     (("__init__" ([☠ dynamic]) dynamic)
      ("__getitem__" ([☠ dynamic]) dynamic)))]
  [(lookup-class Ψ "set")
   (class dict "object"
     ()
     (("__init__" ([☠ dynamic]) dynamic)
      ("__contains__" ([☠ dynamic]) (instancesof "bool"))))]
  [(lookup-class Ψ "None")
   (class None "object" () ())]
  [(lookup-class Ψ ("CheckedDict" cid_key cid_val))
   (class ("CheckedDict" cid_key cid_val)
     "object"
     ()
     (("__init__" ([☠ dynamic]) dynamic)
      ("__getitem__" ([☠ (instancesof cid_key)]) (instancesof cid_val))))]
  ;; lookup user-defined classes
  [(lookup-class Ψ number)
   (lookup Ψ number)])

(define-metafunction SP-statics
  flatten-class* : Ψ cid ... -> flat-class
  [(flatten-class* Ψ) (() ())]
  [(flatten-class* Ψ cid_1 cid_2 ...)
   ((any_fld1 ... any_fld2 ...)
    (any_mth1 ... any_mth2 ...))
   (where ((any_fld1 ...)
           (any_mth1 ...))
          (flatten-class Ψ cid_1))
   (where ((any_fld2 ...)
           (any_mth2 ...))
          (flatten-class* Ψ cid_2 ...))])
(define-metafunction SP-statics
  flatten-class : Ψ cid+dynamic+☠ -> flat-class
  [(flatten-class Ψ cid_slf)
   ((any_slffld ... any_parfld ...)
    (any_slfmth ... any_parmth ...))
   (where (class any cid+dynamic+☠ (any_slffld ...) (any_slfmth ...))
          (lookup-class Ψ cid_slf))
   (where ((any_parfld ...)
           (any_parmth ...))
          (flatten-class Ψ cid+dynamic+☠))]
  [(flatten-class Ψ ☠) (() ())]
  [(flatten-class Ψ dynamic) (() ())])

(module+ test
  (test-equal (term (lookup-member () (instancesof ("CheckedDict" "str" "int")) "__getitem__"))
              (term (-> ([☠ (instancesof "str")]) (instancesof "int")))))
(define-metafunction SP-statics
  lookup-member : Ψ T string -> T+☠
  [(lookup-member Ψ (instancesof cid) string)
   T_mem
   (where C (lookup-class Ψ cid))
   (where (([string_fld T_fld] ...)
           ([string_mth ([x+☠_arg T_arg] ...) T_ret] ...))
          (flatten-class Ψ cid))
   (where T_mem
          (lookup ([string_fld T_fld] ...
                   [string_mth (-> ([x+☠_arg T_arg] ...) T_ret)] ...
                   [string dynamic])
                  string))]
  [(lookup-member Ψ dynamic string) dynamic]
  [(lookup-member Ψ T string) ☠])

(module+ test
  (check-judgment-holds*
   (constructor-ofo (base-Ψ) "object" ())
   (constructor-ofo (base-Ψ) "int" ([☠ dynamic]))
   (constructor-ofo (base-Ψ) "dict" ([☠ dynamic]))
   (constructor-ofo (base-Ψ) "set" ([☠ dynamic]))
   (constructor-ofo (base-Ψ) ("CheckedDict" "str" "int") ([☠ dynamic]))))
(define-judgment-form SP-statics
  #:mode (constructor-ofo I I O)
  #:contract (constructor-ofo Ψ cid ([x+☠_arg T_arg] ...))

  [(where (-> ([x+☠_arg T_arg] ...) T_ret) (lookup-member Ψ (instancesof cid) "__init__"))
   ---------------
   (constructor-ofo Ψ cid ([x+☠_arg T_arg] ...))])