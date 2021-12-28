#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(require "desugar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(define-extended-language SP-compiled SP-core

  ;; program
  (program- level-)

  ;; global names (labels) of global things
  ;;   we will extend l when we reach dynamics.rkt
  (l x
     (chkdict checkable-T checkable-T)
     (tuple (checkable-T ...))
     (user-defined-class x)
     (method class-l x)
     (con c))

  ;; values are just references to global things
  (v (ref l))

  ;; compiled expressions are similar to expressions,
  ;;   but with annotations and boolean operators removed,
  ;;   and with additional constructs:
  ;;     - v, values
  ;;     - let, handy for optimizations
  ;;   and seperating safe and fast operations
  (e- v
      x
      (con c)
      (tuple (e- ...))
      (set (e- ...))
      (dict ([e- e-] ...))
      (is e- e-)
      (if-exp e- e- e-)
      ;; two kinds of attribute!
      (attribute mode e- x)
      ;; three kinds of call!
      (invoke-function l (e- ...))
      ;; the first e- is the object
      (invoke-method l x e- (e- ...))
      ;; other calls
      (call-function e- (e- ...))
      ;; annotations are removed!
      ;;   the s- is the check to be perform on arguments
      (lambda (x ...) s- level-)
      (class x (e- ...) ;; name and parent classes
        ;; class-level members and corresponding checks
        ([x s-] ...)
        ;; instance-level members and corresponding checks
        ([x s-] ...))
      ;; new construct
      (raise-error any)
      (new l (e- ...)))

  ;; statements
  (s- (expr e-)
      (return e-)
      (begin s- ...)
      (if e- s- s-)
      (while e- s- s-)
      break
      continue
      (delete x)
      (delete (attribute mode e- x))
      ;; annotations are removed!
      (assign x e-)
      (assign (attribute mode e- x) e-)
      (import-from x x)
      (try s- e- x s- s-)
      (finally s- s-)
      (raise e-))

  ;; maybe statement
  (s-+☠ s- ☠)

  ;; attribute-access mode
  (mode fast safe)

  (m- (field x)
      (field x e-)
      (method x x (x ...) s- level-))

  (level- (local (x ...) s-))

  ;; a global environments that stores user-defined-classes
  ;;   the x's must be names of user-defined classes
  (Ψ ([l C+☠] ...))

  (C (class l*+dynamic Γ ρ Γ))
  (C+☠ C ☠)
  (l*+dynamic
   (l ...)
   ;; subclass from a dynamic or unsupported class
   dynamic)

  ;; a local environment that maps variables to their types
  (Γ ([x T+☠] ...))

  ;; a local environment that maps names to labels
  (ρ ([x l+☠] ...))

  ;; types
  (T dynamic
     (exactness class-l)
     (Optional nonnonable-T)
     (ClassVar classvarable-T)
     (Final T)
     (-> (T ...) T)
     ;; The remaining are not really types.
     ;;   Each of them only has one inhabitant
     (Type T)
     type-op)

  ;; maybe type
  (T+☠ T ☠)
  (l+☠ l ☠)
  ;; maybe type
  (T? (yes T) (no ☠))
  ;; maybe maybe type
  (T?? (yes T?) (no ☠))

  ;; type-op
  (type-op "CheckedDict[_,_]"
           "Optional[_]"
           "Final[_]"
           "ClassVar[_]"
           "cast"
           (Union class-l))

  (exactness exact subof)

  ;; Types that can go into ClassVar[_]
  (classvarable-T dynamic
                  (exactness class-l)
                  (Optional nonnonable-T))

  ;; Checkable types are real types but without functions
  (checkable-T
   dynamic
   (exactness class-l)
   (Optional nonnonable-and-checkable-T)
   (Type T)
   type-op
   cast)

  (nonnonable-and-checkable-T
   (exactness nonnonable-class-l)
   (Type T)
   cast)

  ;; Non-nonable types are types but without dynamic
  ;;   (exactness None), and Optional
  (nonnonable-T
   (exactness nonnonable-class-l)
   (-> (T ...) T)
   (Type T)
   cast)

  ;; Instanciable types are exact classes
  (instanciable-T (exact class-l))

  ;; class labels are l's but without constants and methods
  ;;   and x is limited to builtin classes
  (class-l
   prim-class-l
   (user-defined-class x))

  (nonnonable-class-l
   nonnonable-prim-class-l
   (user-defined-class x))

  (prim-class-l
   "NoneType"
   nonnonable-prim-class-l)

  ;; non-nonable class identifiers are class labels without none
  (nonnonable-prim-class-l
   "object"
   "int"
   "bool"
   "str"
   "set"
   "list"
   "tuple"
   "dict"
   "type"
   "Exception"
   "Optional[_]"
   "Final[_]"
   "ClassVar[_]"
   "CheckedDict[_,_]"
   (tuple (checkable-T ...))
   (chkdict checkable-T checkable-T))

  ;; utilities
  (program-+☠ program- ☠)
  (xT [x T])
  )

(define-metafunction SP-compiled
  let : ([x e-] ...) s- -> e-
  [(let ([x e-] ...) s-)
   (call-function (lambda (x ...) (begin) (local (x ...) s-))
                  (e- ...))])

(define-metafunction SP-compiled
  lookup-Ψ : Ψ class-l -> (class l*+dynamic Γ ρ Γ)
  ;; Find the meaning of a class by its cid
  ;; lookup user-defined classes
  [(lookup-Ψ Ψ (user-defined-class x))
   (lookup Ψ (user-defined-class x))]
  [(lookup-Ψ Ψ l)
   (lookup-builtin-class l)])
(define-metafunction SP-compiled
  lookup-builtin-class : class-l -> (class l*+dynamic Γ ρ Γ)
  ;; primitive/builtin classes are handled here
  [(lookup-builtin-class "CheckedDict[_,_]")
   (class ("object")
     (["__getitem__" "CheckedDict[_,_]"])
     (["__getitem__" (method "CheckedDict[_,_]" "__getitem__")])
     ())]
  [(lookup-builtin-class "Optional[_]")
   (class ("object")
     (["__getitem__" "Optional[_]"])
     (["__getitem__" (method "Optional[_]" "__getitem__")])
     ())]
  [(lookup-builtin-class "Final[_]")
   (class ("object")
     (["__getitem__" "Final[_]"])
     (["__getitem__" (method "Final[_]" "__getitem__")])
     ())]
  [(lookup-builtin-class "ClassVar[_]")
   (class ("object")
     (["__getitem__" "ClassVar[_]"])
     (["__getitem__" (method "ClassVar[_]" "__getitem__")])
     ())]
  [(lookup-builtin-class "object")
   (class ()
     (["__init__" (-> () dynamic)])
     (["__init__" (method "object" "__init__")])
     ())]
  [(lookup-builtin-class "type")
   (class ("object")
     ()
     ()
     ())]
  [(lookup-builtin-class "Exception")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)])
     (["__init__" (method "Exception" "__init__")])
     ())]
  [(lookup-builtin-class "int")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["__gt__" (-> ((subof "int")) (subof "bool"))]
      ["__lt__" (-> ((subof "int")) (subof "bool"))]
      ["__eq__" (-> ((subof "int")) (subof "bool"))]
      ["__le__" (-> ((subof "int")) (subof "bool"))]
      ["__ge__" (-> ((subof "int")) (subof "bool"))]
      ["__neg__" (-> (dynamic) (subof "bool"))]
      ["__add__" (-> ((subof "int")) (subof "int"))]
      ["__sub__" (-> ((subof "int")) (subof "int"))]
      ["__mul__" (-> ((subof "int")) (subof "int"))]
      ["__div__" (-> ((subof "int")) (subof "int"))]
      ["bit_length" (-> () (subof "int"))])
     (["__init__" (method "int" "__init__")]
      ["__gt__" (method "int" "__gt__")]
      ["__lt__" (method "int" "__lt__")]
      ["__eq__" (method "int" "__eq__")]
      ["__le__" (method "int" "__le__")]
      ["__ge__" (method "int" "__ge__")]
      ["__neg__" (method "int" "__neg__")]
      ["__add__" (method "int" "__add__")]
      ["__sub__" (method "int" "__sub__")]
      ["__mul__" (method "int" "__mul__")]
      ["__div__" (method "int" "__div__")]
      ["bit_length" (method "int" "bit_length")])
     ())]
  [(lookup-builtin-class "bool")
   (class ("int")
     ()
     ()
     ())]
  [(lookup-builtin-class "str")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["__eq__" (-> ((subof "str")) (exact "bool"))]
      ;; can't type optional arguments
      ["split" dynamic])
     (["__init__" (method "str" "__init__")]
      ["__eq__" (method "str" "__eq__")]
      ["split" (method "str" "split")])
     ())]
  [(lookup-builtin-class "dict")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["__getitem__" (-> (dynamic) dynamic)]
      ["__setitem__" (-> (dynamic dynamic) (subof "NoneType"))]
      ["__delitem__" (-> (dynamic) (subof "NoneType"))]
      ["get" (-> (dynamic) dynamic)])
     (["__init__" (method "dict" "__init__")]
      ["__getitem__" (method "dict" "__getitem__")]
      ["__setitem__" (method "dict" "__setitem__")]
      ["__delitem__" (method "dict" "__delitem__")]
      ["get" (method "dict" "get")])
     ())]
  [(lookup-builtin-class "set")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["__contains__" (-> (dynamic) (subof "bool"))])
     (["__init__" (method "set" "__init__")]
      ["__contains__" (method "set" "__contains__")])
     ())]
  [(lookup-builtin-class "list")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["append" (-> (dynamic) (subof "list"))])
     (["__init__" (method "list" "__init__")]
      ["append" (method "list" "append")])
     ())]
  [(lookup-builtin-class "NoneType")
   (class ("object")
     ()
     ()
     ())]
  [(lookup-builtin-class (chkdict T_key T_val))
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["get" (-> (T_key) (union (base-Ψ) (subof "NoneType") T_val))]
      ["keys" (-> () (subof "list"))]
      ["__getitem__" (-> (T_key) T_val)]
      ["__setitem__" (-> (T_key T_val) (subof "NoneType"))]
      ["__delitem__" (-> (T_key) (subof "NoneType"))]
      ["setdefault" (-> (T_key T_val) (subof "NoneType"))])
     (["__init__" (method (chkdict T_key T_val) "__init__")]
      ["get" (method (chkdict T_key T_val) "get")]
      ["keys" (method (chkdict T_key T_val) "keys")]
      ["__getitem__" (method (chkdict T_key T_val) "__getitem__")]
      ["__setitem__" (method (chkdict T_key T_val) "__setitem__")]
      ["__delitem__" (method (chkdict T_key T_val) "__delitem__")]
      ["setdefault" (method (chkdict T_key T_val) "setdefault")])
     ())])

(define-metafunction SP-compiled
  base-Ψ : -> Ψ
  [(base-Ψ) ()])

(define-metafunction SP-compiled
  base-Γ : -> Γ
  [(base-Γ)
   (["object" (Type (subof "object"))]
    ["int" (Type (subof "int"))]
    ["bool" (Type (subof "bool"))]
    ["str" (Type (subof "str"))]
    ["dict" (Type (subof "dict"))]
    ["set" (Type (subof "set"))]
    ["type" (Type (subof "type"))]
    ["isinstance" (-> (dynamic dynamic) dynamic)]
    ["len" (-> (dynamic) dynamic)]
    ["Exception" (Type (subof "Exception"))]
    ["max" dynamic]
    ["min" dynamic]
    ["issubclass" (-> (dynamic dynamic) (exact "bool"))])])

(define-metafunction SP-compiled
  prim-Γ : -> Γ
  [(prim-Γ)
   (extend (base-Γ)
           ["CheckedDict" (Type (subof "CheckedDict[_,_]"))]
           ["PyDict" (Type (subof "dict"))]
           ["Optional" (Type (subof "Optional[_]"))])])

(module+ test
  (test-equal (term (T-of-c None))
              (term (exact "NoneType")))
  (test-equal (term (T-of-c #t))
              (term (exact "bool")))
  (test-equal (term (T-of-c 2))
              (term (exact "int")))
  (test-equal (term (T-of-c "foo"))
              (term (exact "str"))))
(define-metafunction SP-compiled
  T-of-c : c -> T
  [(T-of-c c) (exact (l-of-c c))])
(define-metafunction SP-compiled
  l-of-c : c -> l
  [(l-of-c None) "NoneType"]
  [(l-of-c boolean) "bool"]
  [(l-of-c integer) "int"]
  [(l-of-c string) "str"])

(define-judgment-form SP-compiled
  #:mode (⊢Ψ I)
  #:contract (⊢Ψ Ψ)
  [(where ([l C] ...) Ψ)
   (Ψ⊢l Ψ l) ...
   -----------------------
   (⊢Ψ Ψ)])
(define-judgment-form SP-compiled
  #:mode (Ψ⊢l I I)
  #:contract (Ψ⊢l Ψ l)
  [(where Γ_cls (flatten #t Ψ l))
   (where Γ_ins (flatten #f Ψ l))
   (class-Ψ⊢Γ Ψ Γ_cls)
   (class-Ψ⊢Γ Ψ Γ_ins)
   (where #t (disjoint (keysof Γ_cls) (keysof Γ_ins)))
   ------------------------------
   (Ψ⊢l Ψ l)])
(module+ test
  (test-equal (term #t) (term (disjoint ("a" "b") ("c"))))
  (test-equal (term #f) (term (disjoint ("a" "b") ("c" "a")))))
(define-metafunction SP-compiled
  disjoint : (x ...) (x ...) -> boolean
  [(disjoint (x_1 ... x x_2 ...) (x_3 ... x x_4 ...)) #f]
  [(disjoint (x_1 ...) (x_2 ...)) #t])
(define-metafunction SP-compiled
  keysof : ([x any] ...) -> (x ...)
  [(keysof ([x any] ...)) (x ...)])
(define-judgment-form SP-compiled
  #:mode (class-Ψ⊢Γ I I)
  #:contract (class-Ψ⊢Γ Ψ Γ)
  [(where #f (bad-flat-class Ψ Γ))
   ---------------------------------
   (class-Ψ⊢Γ Ψ Γ)])
(define-judgment-form SP-compiled
  #:mode (bad-flat-class I I)
  #:contract (bad-flat-class Ψ Γ)
  
  [(where #f (good-shadow Ψ x T_1 T_2))
   -----------------------------------------------------------------------
   (bad-flat-class Ψ (xT_0 ... [x T_1] xT_1 ... [x T_2] xT_2 ...))])
(module+ test
  (test-equal (term #t) (term (good-shadow (base-Ψ) "abc" (subof "int") (subof "int"))))
  (test-equal (term #f) (term (good-shadow (base-Ψ) "abc" (subof "bool") (subof "int"))))
  (test-equal (term #f) (term (good-shadow (base-Ψ) "abc" (subof "int") (subof "bool"))))
  (test-equal (term #t) (term (good-shadow (base-Ψ) "abc"
                                           (-> ((subof "int")) (subof "bool"))
                                           (-> ((subof "bool")) (subof "int"))))))
(define-metafunction SP-compiled
  good-shadow : Ψ x T T -> boolean
  [(good-shadow Ψ "__init__" (-> (T_dom1 ...) T_cod1) (-> (T_dom2 ...) T_cod2))
   #t]
  [(good-shadow Ψ x (-> (T_dom1 ...) T_cod1) (-> (T_dom2 ...) T_cod2))
   (fun<: Ψ (-> (T_dom1 ...) T_cod1) (-> (T_dom2 ...) T_cod2))]
  [(good-shadow Ψ x T_1 T_2)
   ,(equal? (term T_1) (term T_2))])
(define-metafunction SP-compiled
  fun<: : Ψ (-> (T ...) T) (-> (T ...) T) -> boolean
  [(fun<: Ψ (-> (T_dom1 ...) T_cod1) (-> (T_dom2 ...) T_cod2))
   #t
   (where #t ,(= (length (term (T_dom1 ...)))
                 (length (term (T_dom2 ...)))))
   (where (#t ...) ((Ψ⊢T<:T Ψ T_dom2 T_dom1) ...))
   (where #t (Ψ⊢T<:T Ψ T_cod1 T_cod2))]
  [(fun<: Ψ (-> (T_dom1 ...) T_cod1) (-> (T_dom2 ...) T_cod2))
   #f])
(define-metafunction SP-compiled
  flatten : boolean Ψ l -> Γ
  [(flatten boolean Ψ l)
   (append Γ (flatten-l*+dynamic boolean Ψ l*+dynamic))
   (where (class l*+dynamic Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l))
   (where #t boolean)
   (where Γ Γ_cls)]
  [(flatten boolean Ψ l)
   (append Γ (flatten-l*+dynamic boolean Ψ l*+dynamic))
   (where (class l*+dynamic Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l))
   (where #f boolean)
   (where Γ Γ_ins)])
(define-metafunction SP-compiled
  flatten-l*+dynamic : boolean Ψ l*+dynamic -> Γ
  [(flatten-l*+dynamic boolean Ψ (l ...))
   (flatten-l* boolean Ψ l ...)]
  [(flatten-l*+dynamic boolean Ψ dynamic)
   ()])
(define-metafunction SP-compiled
  flatten-l* : boolean Ψ l ... -> Γ
  [(flatten-l* boolean Ψ) ()]
  [(flatten-l* boolean Ψ l_0 l_1 ...)
   (append (flatten boolean Ψ l_0)
           (flatten-l* boolean Ψ l_1 ...))])

(module+ test
  (check-judgment-holds*
   (Ψ⊢class-l<:class-l () "NoneType" "object")
   (Ψ⊢class-l<:class-l () "NoneType" "NoneType")
   (Ψ⊢class-l<:class-l () "bool" "int"))
  (check-not-judgment-holds*
   (Ψ⊢class-l<:class-l () "object" "NoneType")))
(define-judgment-form SP-compiled
  #:mode (Ψ⊢class-l<:class-l I I I)
  #:contract (Ψ⊢class-l<:class-l Ψ class-l class-l)
  ;; subclass relation

  [------------------------- "object-is-the-top"
   (Ψ⊢class-l<:class-l Ψ l "object")]

  [------------------------- "refl"
   (Ψ⊢class-l<:class-l Ψ l l)]

  [(where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_lft))
   (member #t ((Ψ⊢class-l<:class-l Ψ l_sup l_rht) ...))
   ------------------------ "super"
   (Ψ⊢class-l<:class-l Ψ l_lft l_rht)])

(module+ test
  (test-equal (term (union (base-Ψ) (subof "int") (subof "int")))
              (term (subof "int")))
  (test-equal (term (union (base-Ψ) (subof "int") dynamic))
              (term dynamic))
  (test-equal (term (union (base-Ψ) dynamic (subof "int")))
              (term dynamic))
  (test-equal (term (union (base-Ψ) (subof "int") (subof "str")))
              (term dynamic))
  (test-equal (term (union (base-Ψ) (subof "int") (subof "NoneType")))
              (term (Optional (subof "int"))))
  (test-equal (term (union (base-Ψ) (subof "NoneType") (subof "int")))
              (term (Optional (subof "int"))))
  (test-equal (term (union (base-Ψ) (subof "NoneType") (Optional (subof "int"))))
              (term (Optional (subof "int"))))
  (test-equal (term (union (base-Ψ) (Optional (subof "int")) (subof "NoneType")))
              (term (Optional (subof "int"))))
  (test-equal (term (union (base-Ψ) (Optional (exact "int")) (exact "int")))
              (term (Optional (exact "int")))))
(define-metafunction SP-compiled
  union : Ψ T T -> T
  ;; refl
  [(union Ψ T T) T]
  ;; Optional
  [(union Ψ nonnonable-T (subof "NoneType"))
   (Optional nonnonable-T)]
  [(union Ψ (subof "NoneType") nonnonable-T)
   (Optional nonnonable-T)]
  [(union Ψ (Optional T) (subof "NoneType")) (Optional T)]
  [(union Ψ (subof "NoneType") (Optional T)) (Optional T)]
  [(union Ψ (Optional T_1) (Optional T_2))
   (union Ψ (subof "NoneType") (union Ψ T_1 T_2))]
  [(union Ψ T_1 (Optional T_2))
   (union Ψ (union Ψ T_1 T_2) (subof "NoneType"))]
  [(union Ψ (Optional T_1) T_2)
   (union Ψ (union Ψ T_1 T_2) (subof "NoneType"))]
  ;; otherwise, exact classes are useless
  [(union Ψ (exact l) T)
   (union Ψ (subof l) T)]
  [(union Ψ T (exact l))
   (union Ψ T (subof l))]
  ;; merge into parent class
  [(union Ψ (subof l_1) (subof l_2))
   (subof l_2)
   (judgment-holds (Ψ⊢class-l<:class-l Ψ l_1 l_2))]
  [(union Ψ (subof l_1) (subof l_2))
   (subof l_1)
   (judgment-holds (Ψ⊢class-l<:class-l Ψ l_2 l_1))]
  [(union Ψ T_1 T_2) dynamic])

(module+ test
  (check-judgment-holds* (Ψ⊢T<:T (base-Ψ) (Optional (exact "int")) (Optional (subof "int")))))
(define-judgment-form SP-compiled
  #:mode (Ψ⊢T<:T I I I)
  #:contract (Ψ⊢T<:T Ψ T T)

  [(where T_dst (union Ψ T_src T_dst))
   -----------------
   (Ψ⊢T<:T Ψ T_src T_dst)])

(define-judgment-form SP-compiled
  #:mode (Ψ⊢T≲:T I I I)
  #:contract (Ψ⊢T≲:T Ψ T T)

  [--------------------
   (Ψ⊢T≲:T Ψ dynamic T)]

  [--------------------
   (Ψ⊢T≲:T Ψ T dynamic)]

  [(Ψ⊢T<:T Ψ T_src T_dst)
   -----------------------
   (Ψ⊢T≲:T Ψ T_src T_dst)])

(define-metafunction SP-compiled
  intersection : Ψ T T -> T+☠
  [(intersection Ψ T_1 T_2)
   T_1
   (judgment-holds (Ψ⊢T<:T Ψ T_1 T_2))]
  [(intersection Ψ T_1 T_2)
   T_2
   (judgment-holds (Ψ⊢T<:T Ψ T_2 T_1))]
  [(intersection Ψ (Optional T_1) (Optional T_2))
   (Optional T)
   (where T (intersection Ψ T_1 T_2))]
  [(intersection Ψ (Optional T_1) (Optional T_2))
   (subof "NoneType")
   (where ☠ (intersection Ψ T_1 T_2))]
  [(intersection Ψ (Final T_1) T_2)
   (Final T)
   (where T (intersection Ψ T_2 T_1))]
  [(intersection Ψ T_1 T_2) ☠])

(define-metafunction SP-compiled
  remove-None : T -> T
  [(remove-None (Optional T)) T]
  [(remove-None (subof "NoneType")) dynamic]  ;; If we want to be pedantic, should be bottom
  [(remove-None T) T])

(module+ test
  (test-equal (term (lookup-member-T (base-Ψ) "bool" "__add__"))
              (term (-> ((subof "int")) (subof "int")))))
(define-metafunction SP-compiled
  lookup-member-T : Ψ l x -> T+☠
  ;; Given a class environment Ψ, a class name l, a member name x, what is the type of
  ;;   that member when accessed from an instance of the class?
  
  ;; a hack to support __or__ union type
  [(lookup-member-T Ψ l_cls "__or__")
   (Union l_cls)]
  ;; if in the Γ_ins of the current class, return the type
  [(lookup-member-T Ψ l_cls x)
   T
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes T) (lookup? Γ_ins x))]
  ;; if in the current class, return the type
  [(lookup-member-T Ψ l_cls x)
   T
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes T) (lookup? Γ_cls x))]
  ;; if there is only one parent, go to that parent
  [(lookup-member-T Ψ l_cls x)
   (lookup-member-T Ψ l_sup x)
   (where (class (l_sup) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))]
  [(lookup-member-T Ψ l x)
   ☠])
(define-metafunction SP-compiled
  lookup-writable-member-T : Ψ l x -> T??
  ;; Given a class environment Ψ, a class name l, a member name x,
  ;;   is the member possibly writable? If yes, is the field declared?
  
  ;; if in the Γ_ins of the current class, return the type
  [(lookup-writable-member-T Ψ l_cls x)
   (yes (yes T))
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes T) (lookup? Γ_ins x))]
  ;; if in the Γ_cls of the current class, the member is not writable
  ;;   at the instance level
  [(lookup-writable-member-T Ψ l_cls x)
   (no ☠)
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes T) (lookup? Γ_cls x))]
  ;; if there is only one parent, go to that parent
  [(lookup-writable-member-T Ψ l_cls x)
   (lookup-writable-member-T Ψ l_sup x)
   (where (class (l_sup) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))]
  ;; In all other cases, the member is potentially writable but undeclared
  [(lookup-writable-member-T Ψ l x)
   (yes (no ☠))])
(define-metafunction SP-compiled
  lookup-class-var-T : Ψ l x -> T+☠
  ;; Given a class environment Ψ, a class name l, a member name x, what is the type of
  ;;   that member when accessed from an instance of the class?
  
  ;; if in the Γ_ins of the current class, return the type
  [(lookup-class-var-T Ψ l_cls x)
   ☠
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes T) (lookup? Γ_ins x))]
  ;; if in the current class, return the type
  [(lookup-class-var-T Ψ l_cls x)
   T
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes T) (lookup? Γ_cls x))]
  ;; if there is only one parent, go to that parent
  [(lookup-class-var-T Ψ l_cls x)
   (lookup-class-var-T Ψ l_sup x)
   (where (class (l_sup) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))]
  [(lookup-class-var-T Ψ l x)
   ☠])

(define-metafunction SP-compiled
  lookup-member-l : Ψ l x -> l
  ;; a hack to support __or__ union type
  [(lookup-member-l Ψ l_cls "__or__")
   (method l_cls "__or__")]
  ;; if in the current class, return the type
  [(lookup-member-l Ψ l_cls x)
   l
   (where (class (l_sup ...) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))
   (where (yes l) (lookup? ρ_cls x))]
  ;; if there is only one parent, go to that parent
  [(lookup-member-l Ψ l_cls x)
   (lookup-member-l Ψ l_sup x)
   (where (class (l_sup) Γ_cls ρ_cls Γ_ins) (lookup-Ψ Ψ l_cls))])


(module+ test
  (test-equal (term (eval-t (base-Ψ) (prim-Γ) dynamic))
              (term dynamic))
  (test-equal (term (eval-t (base-Ψ) (prim-Γ) "int"))
              (term (subof "int")))
  (test-equal (term (eval-t (base-Ψ) (prim-Γ)
                            (desugar-e (subscript "CheckedDict" (tuple ("int" "str"))))))
              (term (subof (chkdict (subof "int") (subof "str"))))))
(define-metafunction SP-compiled
  eval-t : Ψ Γ t -> T
  ;; the dynamic cases
  [(eval-t Ψ Γ dynamic)
   dynamic]
  ;; If we got an type object, we know it represents the type
  ;; special case
  [(eval-t Ψ Γ (con x))
   (eval-t Ψ Γ x)]
  ;; normal case
  [(eval-t Ψ Γ e)
   (T-of-T T)
   (where [e- T] (compile-e Ψ Γ Γ e))])
;; T-of-T answers the following question:
;;   when an expression is used in a type context and the expression has type T
;;   what is the type that the expression is referring to?
(define-metafunction SP-compiled
  T-of-T : T -> T
  [(T-of-T (Type T)) T]
  [(T-of-T (exact "NoneType")) (subof "NoneType")]
  [(T-of-T dynamic) dynamic])

(module+ test
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) "int"))
              (term ["int" (Type (subof "int"))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (con "foo")))
              (term [(ref (con "foo")) (exact "str")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (tuple ("int" "str"))))
              (term [(tuple ("int" "str"))
                     (exact (tuple ((Type (subof "int")) (Type (subof "str")))))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (set ())))
              (term [(set ()) (exact "set")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (dict ())))
              (term [(dict ()) (exact "dict")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (not (con None))))
              (term [(if-exp (ref (con None)) (ref (con #f)) (ref (con #t)))
                     (exact "bool")]))
  (test-match SP-compiled
              [(call-function (lambda (x_tmp) (begin) (local (x_tmp) (return (if-exp x_tmp (ref (con 2)) x_tmp)))) ((ref (con 1))))
               dynamic]
              (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (and (con 1) (con 2)))))
  (test-match SP-compiled
              [(call-function (lambda (x_tmp) (begin) (local (x_tmp) (return (if-exp x_tmp x_tmp (ref (con 2)))))) ((ref (con 1))))
               dynamic]
              (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (or (con 1) (con 2)))))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (is "int" "bool")))
              (term [(is "int" "bool")
                     (exact "bool")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (if-exp "int" "bool" (con None))))
              (term [(if-exp "int" "bool" (ref (con None)))
                     (Optional (Type (subof "bool")))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (attribute "bool" "__add__")))
              (term [(attribute safe "bool" "__add__") dynamic]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (call "int" ("bool"))))
              (term [(new "int" ("bool"))
                     (exact "int")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (lambda () (con 2))))
              (term [(lambda () (begin) (local () (return (ref (con 2)))))
                     (-> () (exact "int"))])))
;; We do need two type environment to handle occurrance typing
(define-metafunction SP-compiled
  compile-e : Ψ Γ Γ e -> [e- T]
  ;; variable
  [(compile-e Ψ Γ_dcl Γ_lcl x)
   [x T]
   (where T (lookup Γ_lcl x))]
  ;; constant
  [(compile-e Ψ Γ_dcl Γ_lcl (con c))
   [(ref (con c))
    (T-of-c c)]]
  ;; tuple literal
  [(compile-e Ψ Γ_dcl Γ_lcl (tuple (e ...)))
   [(tuple (e- ...))
    (exact (tuple (T ...)))]
   (where ([e- T] ...) ((compile-e Ψ Γ_dcl Γ_lcl e) ...))]
  ;; set literal
  [(compile-e Ψ Γ_dcl Γ_lcl (set (e ...)))
   [(set ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) ...))
    (exact "set")]]
  ;; dict literal
  [(compile-e Ψ Γ_dcl Γ_lcl (dict ([e_key e_val] ...)))
   [(dict ([(as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_key))
                   (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_val))] ...))
    (exact "dict")]]
  ;; not
  [(compile-e Ψ Γ_dcl Γ_lcl (not e))
   (compile-e Ψ Γ_dcl Γ_lcl (if-exp e (con #f) (con #t)))]
  ;; and
  [(compile-e Ψ Γ_dcl Γ_lcl (and e_1 e_2))
   (compile-e Ψ Γ_dcl Γ_lcl (if-exp e_1 e_2 (con #f)))]
  ;; or
  [(compile-e Ψ Γ_dcl Γ_lcl (or e_1 e_2))
   (compile-e Ψ Γ_dcl Γ_lcl (if-exp e_1 (con #t) e_2))]
  #;
  [(compile-e Ψ Γ_dcl Γ_lcl (and e_1 e_2))
   (compile-e Ψ Γ_dcl Γ_lcl (call (lambda ([x_tmp dynamic]) (if-exp x_tmp e_2 x_tmp)) (e_1)))
   (where x_tmp ,(symbol->string (gensym 'and)))]
  #;
  [(compile-e Ψ Γ_dcl Γ_lcl (or e_1 e_2))
   (compile-e Ψ Γ_dcl Γ_lcl (call (lambda ([x_tmp dynamic]) (if-exp x_tmp x_tmp e_2)) (e_1)))
   (where x_tmp ,(symbol->string (gensym 'or)))]
  ;; is
  [(compile-e Ψ Γ_dcl Γ_lcl (is e_1 e_2))
   [(is (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_1)) (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_2)))
    (exact "bool")]]
  ;; if-exp
  [(compile-e Ψ Γ_dcl Γ_lcl (if-exp e_cnd e_thn e_els))
   (compile-e-if-exp Ψ Γ_dcl Γ_lcl e_cnd e_thn e_els)]
  ;; attribute
  [(compile-e Ψ Γ_dcl Γ_lcl (attribute e x))
   (compile-e-attribute Ψ Γ_dcl Γ_lcl e x)]
  ;; call
  [(compile-e Ψ Γ_dcl Γ_lcl (call e_fun (e_arg ...)))
   (compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_arg ...))]
  ;; lambda, IT IS IMPORTANT TO USE Γ_dcl!!!
  [(compile-e Ψ Γ_dcl Γ_lcl (lambda ([x_arg t_arg] ...) e_out))
   (compile-e-lambda Ψ Γ_dcl Γ_lcl ([x_arg t_arg] ...) e_out)])
(define-metafunction SP-compiled
  maybe-cast : Ψ [e- T] T -> e-
  [(maybe-cast Ψ [e-_src T_src] T_dst)
   e-_src
   (judgment-holds (Ψ⊢T<:T Ψ T_src T_dst))]
  [(maybe-cast Ψ [e-_src T_src] checkable-T_dst)
   (let (["tmp" e-_src])
     (begin
       (compile-check "tmp" checkable-T_dst)
       (return "tmp")))
   (judgment-holds (Ψ⊢T≲:T Ψ T_src checkable-T_dst))])
(define-metafunction SP-compiled
  compile-e-lambda : Ψ Γ Γ ([x t] ...) e -> [e- T]
  [(compile-e-lambda Ψ Γ_dcl Γ_lcl ([x_arg t_arg] ...) e_out)
   [(lambda (x_arg ...)
      (make-begin (compile-check x_arg T_arg) ...)
      (local (x_arg ...) (return e-_out)))
    (-> (T_arg ...) T_out)]
   (where (T_arg ...) ((eval-t Ψ Γ_dcl t_arg) ...))
   (where [e-_out T_out] (compile-e Ψ Γ_dcl (extend Γ_dcl [x_arg T_arg] ...) e_out))])
(module+ test
  (test-equal (term (type-call (base-Ψ) "CheckedDict[_,_]" (exact (tuple ((Type (subof "int"))
                                                                          (Type (subof "str")))))))
              (term (Type (subof (chkdict (subof "int") (subof "str"))))))
  (test-equal (term (type-call (base-Ψ) "Optional[_]" (Type (subof "int"))))
              (term (Type (Optional (subof "int"))))))
(define-metafunction SP-compiled
  type-call : Ψ type-op T -> T
  [(type-call Ψ (Union l_1) T)
   (Type (union (base-Ψ) (subof l_1) (T-of-T T)))]
  [(type-call Ψ "CheckedDict[_,_]" (exact (tuple (T_key T_val))))
   (Type (subof (chkdict (T-of-T T_key) (T-of-T T_val))))]
  [(type-call Ψ "Optional[_]" T)
   (Type (Optional (T-of-T T)))]
  [(type-call Ψ "ClassVar[_]" T)
   (Type (ClassVar (T-of-T T)))]
  [(type-call Ψ "Final[_]" T)
   (Type (Final (T-of-T T)))])
(module+ test
  (test-equal (term (compile-e-call (base-Ψ) (prim-Γ) (prim-Γ)
                                    (attribute (con 2) "__add__") ((con 3))))
              (term [(invoke-function (method "int" "__add__") ((ref (con 2)) (ref (con 3))))
                     (subof "int")]))
  (test-equal (term (compile-e-call (base-Ψ) (prim-Γ) (prim-Γ)
                                    (attribute "CheckedDict" "__getitem__")
                                    ((tuple ("int" "str")))))
              (term [(invoke-function (method "CheckedDict[_,_]" "__getitem__")
                                      ("CheckedDict"
                                       (tuple ("int" "str"))))
                     (Type (subof (chkdict (subof "int") (subof "str"))))])))
(define-metafunction SP-compiled
  compile-e-call : Ψ Γ Γ e (e ...) -> [e- T]
  ;; type-level calls
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   [(invoke-function l_mth (e-_obj e-_arg ...))
    (type-call Ψ type-op T_arg ...)]
   (where [e-_obj (Type (subof l_cls))] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where type-op (lookup-member-T Ψ l_cls x_mth))
   (where l_mth (lookup-member-l Ψ l_cls x_mth))
   (where ([e-_arg T_arg] ...) ((compile-e Ψ Γ_dcl Γ_lcl e_arg) ...))]
  ;; method call
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   (compile-method-calls Ψ Γ_dcl Γ_lcl (compile-e Ψ Γ_dcl Γ_lcl e_obj) x_mth e_arg ...)]
  ;; new instance
  [(compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_arg ...))
   (compile-new Ψ Γ_dcl Γ_lcl T (e_arg ...))
   (where [e-_fun (Type T)] (compile-e Ψ Γ_dcl Γ_lcl e_fun))]
  ;; functions
  [(compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_arg ...))
   [(call-function e-_fun ((maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where [e-_fun (-> (T_arg ...) T_out)] (compile-e Ψ Γ_dcl Γ_lcl e_fun))
   (where #t ,(= (length (term (e_arg ...)))
                 (length (term (T_arg ...)))))]
  ;; the type function
  [(compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_arg))
   [(call-function e-_fun ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg))))
    (Type (subof "type"))]
   (where [e-_fun (Type (subof "type"))] (compile-e Ψ Γ_dcl Γ_lcl e_fun))]
  ;; the cast operator
  [(compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_dst e_val))
   [(maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_val) T_dst)
    T_dst]
   (where [e-_fun "cast"] (compile-e Ψ Γ_dcl Γ_lcl e_fun))
   (where T_dst (eval-t Ψ Γ_dcl e_dst))]
  ;; dynamic
  [(compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_arg ...))
   [(call-function e-_fun ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...))
    dynamic]
   (where [e-_fun dynamic] (compile-e Ψ Γ_dcl Γ_lcl e_fun))])
(define-metafunction SP-compiled
  compile-new : Ψ Γ_dcl Γ_lcl T (e_arg ...) -> [e- T]
  ;; the type function
  [(compile-new Ψ Γ_dcl Γ_lcl (subof "type") (e_arg))
   [(call-function (ref "type") ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg))))
    (Type (subof "type"))]]
  ;; CheckedDict(dict)
  [(compile-new Ψ Γ_dcl Γ_lcl (subof l) ((dict ([e_key e_val] ...))))
   [(new l ((dict ([(maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_key) T_key)
                    (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_val) T_val)]
                   ...))))
    (exact l)]
   (where (chkdict T_key T_val) l)]
  ;; general case
  [(compile-new Ψ Γ_dcl Γ_lcl (subof l) (e_arg ...))
   [(new l ((maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    (exact l)]
   (where (-> (T_arg ...) T_out) (lookup-member-T Ψ l "__init__"))
   (where #t ,(= (length (term (e_arg ...)))
                 (length (term (T_arg ...)))))])
(define-metafunction SP-compiled
  compile-method-calls : Ψ Γ_dcl Γ_lcl [e-_obj T_obj] x_mth e_arg ... -> [e- T]
  ;; exact
  [(compile-method-calls Ψ Γ_dcl Γ_lcl [e-_obj (exact l_cls)] x_mth e_arg ...)
   (compile-exact-method-calls Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ...)]
  ;; subof
  [(compile-method-calls Ψ Γ_dcl Γ_lcl [e-_obj (subof l_cls)] x_mth e_arg ...)
   (compile-subof-method-calls Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ...)]
  ;; dynamic
  [(compile-method-calls Ψ Γ_dcl Γ_lcl [e-_obj T] x_mth e_arg ...)
   [(call-function (attribute safe (as-dyn [e-_obj T]) x_mth) ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...))
    dynamic]
   (judgment-holds (attributable T))])
(define-metafunction SP-compiled
  compile-exact-method-calls : Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ... -> [e- T]
  [(compile-exact-method-calls Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ...)
   [(invoke-function l_mth (e-_obj (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where (-> (T_arg ...) T_out) (lookup-member-T Ψ l_cls x_mth))
   (where #t ,(= (length (term (e_arg ...)))
                 (length (term (T_arg ...)))))
   (where l_mth (lookup-member-l Ψ l_cls x_mth))]
  [(compile-exact-method-calls Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ...)
   [(call-function (attribute safe e-_obj x_mth) ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...))
    dynamic]
   (where dynamic (lookup-member-T Ψ l_cls x_mth))])
(define-metafunction SP-compiled
  compile-subof-method-calls : Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ... -> [e- T]
  [(compile-subof-method-calls Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ...)
   [(invoke-method l_cls x_mth e-_obj ((maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where (-> (T_arg ...) T_out) (lookup-member-T Ψ l_cls x_mth))
   (where #t ,(= (length (term (e_arg ...)))
                 (length (term (T_arg ...)))))]
  [(compile-subof-method-calls Ψ Γ_dcl Γ_lcl l_cls e-_obj x_mth e_arg ...)
   [(call-function (attribute safe e-_obj x_mth) ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...))
    dynamic]
   (where dynamic (lookup-member-T Ψ l_cls x_mth))])
(define-metafunction SP-compiled
  compile-e-attribute : Ψ Γ Γ e x -> [e- T]
  [(compile-e-attribute Ψ Γ_dcl Γ_lcl e x)
   [(attribute mode e- x)
    T]
   (where [mode e- T] (resolve-attribute Ψ Γ_dcl Γ_lcl e x))])
(define-metafunction SP-compiled
  resolve-attribute : Ψ Γ Γ e x -> [mode e- T]
  [(resolve-attribute Ψ Γ_dcl Γ_lcl e x)
   [fast e- T]
   (where [e- (exactness l)] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T (lookup-member-T Ψ l x))]
  [(resolve-attribute Ψ Γ_dcl Γ_lcl e x)
   [safe (as-dyn [e- T]) dynamic]
   (where [e- T] (compile-e Ψ Γ_dcl Γ_lcl e))
   (judgment-holds (attributable T))])
(define-metafunction SP-compiled
  resolve-writable-attribute : Ψ Γ Γ e x -> [mode e- T]
  ;; similar to resolve-attribute, but with a writable test
  ;; Given an instance, the member is writable and declared
  [(resolve-writable-attribute Ψ Γ_dcl Γ_lcl e x)
   [fast e- T]
   (where [e- (exactness l)] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where (yes (yes T)) (lookup-writable-member-T Ψ l x))]
  ;; Given an instance, the member is possibly writable (undeclared)
  [(resolve-writable-attribute Ψ Γ_dcl Γ_lcl e x)
   [safe e- dynamic]
   (where [e- (exactness l)] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where (yes (no ☠)) (lookup-writable-member-T Ψ l x))]
  ;; Given a class
  [(resolve-writable-attribute Ψ Γ_dcl Γ_lcl e x)
   [fast e- T]
   (where [e- (Type (exactness l))] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T (lookup-class-var-T Ψ l x))]
  ;; Given a dynamic
  [(resolve-writable-attribute Ψ Γ_dcl Γ_lcl e x)
   [safe e- dynamic]
   (where [e- dynamic] (compile-e Ψ Γ_dcl Γ_lcl e))])
(define-judgment-form SP-compiled
  #:mode (attributable I)
  #:contract (attributable T)

  [----------------------
   (attributable dynamic)]

  [----------------------
   (attributable (Type T))]

  [(where #f ,(redex-match? SP-compiled "NoneType" (term l)))
   (where #f ,(redex-match? SP-compiled "CheckedDict[_,_]" (term l)))
   (where #f ,(redex-match? SP-compiled "Optional[_]" (term l)))
   (where #f ,(redex-match? SP-compiled "Final[_]" (term l)))
   (where #f ,(redex-match? SP-compiled "ClassVar[_]" (term l)))
   ----------------------
   (attributable (exactness l))])
(define-metafunction SP-compiled
  bind-method : T -> T
  ;; TODO: I am not sure if I want to use T_slf to match
  [(bind-method (-> (T_slf T_arg ...) T_ret))
   (-> (T_arg ...) T_ret)]
  [(bind-method T) T])
(define-metafunction SP-compiled
  compile-e-if-exp : Ψ Γ Γ e e e -> [e- T]
  ;; base case
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl e e_thn e_els)
   [(if-exp (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) e-_thn e-_els)
    (union Ψ T_thn T_els)]
   (where [e- Γ_thn Γ_els] (condition Ψ Γ_dcl Γ_lcl e))
   (where [e-_thn T_thn] (compile-e Ψ Γ_dcl Γ_thn e_thn))
   (where [e-_els T_els] (compile-e Ψ Γ_dcl Γ_els e_els))])

(define-metafunction SP-compiled
  as-dyn : [e- T] -> e-
  [(as-dyn [e- T]) e-])

(define-metafunction SP-compiled
  compile-check : e- T -> s-
  [(compile-check e- dynamic) (begin)]
  [(compile-check e- (-> (T_arg ...) T_ret)) (expr (raise-error "internal error"))]
  [(compile-check e- (exactness l))
   (make-assert
    (check-exactness (invoke-function "type" (e-)) exactness l))]
  [(compile-check e- (Optional T))
   (if (is e- (con None))
       (begin)
       (compile-check e- T))])
(define-metafunction SP-compiled
  check-exactness : e- exactness l -> e-
  [(check-exactness e- exact l)
   (is e- (ref l))]
  [(check-exactness e- subof l)
   (invoke-function "issubclass" (e- (ref l)))])

(module+ test
  ;; expr
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (expr "int")))
              (term (expr "int")))
  ;; return
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) (exact "int")
                               (return (con 2))))
              (term (return (ref (con 2)))))
  ;; assert
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (assert (con #t))))
              (term (if (ref (con #t)) (begin) (expr (raise-error "assertion error")))))
  ;; begin
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (begin (expr "int") (expr "bool"))))
              (term (begin (expr "int") (expr "bool"))))
  ;; if
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) (Optional (exact "int"))
                               (if (con None) (return (con None)) (return (con 2)))))
              (term (if (ref (con None)) (return (ref (con None))) (return (ref (con 2))))))
  ;; delete x
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (delete "int")))
              (term (delete "int")))
  ;; ann x
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (ann "i" "int")))
              (term (begin)))
  ;; ann attribute
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (ann (attribute (con 2) "__add__") "int")))
              (term (begin)))
  ;; ann-assign
  (test-equal (term (compile-s (base-Ψ)
                               (extend (prim-Γ) ["i" (subof "int")])
                               (extend (prim-Γ) ["i" (subof "int")])
                               ☠
                               (ann-assign "i" "int" (con 2))))
              (term (assign "i" (ref (con 2)))))
  ;; function-def
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (function-def "f" (["i" "int"]) "int"
                                             (local (["i" "int"])
                                               (return "i")))))
              (term (assign "f" (lambda ("i")
                                  (begin
                                    (if (invoke-function "issubclass"
                                                         ((invoke-function "type" ("i"))
                                                          (ref "int")))
                                        (begin)
                                        (expr (raise-error "assertion error"))))
                                  (local ("i")
                                    (return "i"))))))
  )
(define-metafunction SP-compiled
  compile-s : Ψ Γ Γ T+☠ s -> s-
  ;; expr
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (expr e))
   (expr (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)))]
  ;; return
  [(compile-s Ψ Γ_dcl Γ_lcl T (return e))
   (return (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e) T))]
  ;; assert
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (assert e))
   (make-assert (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)))]
  ;; begin
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s ...))
   (compile-begin Ψ Γ_dcl Γ_lcl T+☠ s ...)]
  ;; if
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (if e_cnd s_thn s_els))
   (compile-s-if Ψ Γ_dcl Γ_lcl T+☠ e_cnd s_thn s_els)]
  ;; while
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (while e_cnd s_thn s_els))
   (compile-s-while Ψ Γ_dcl Γ_lcl T+☠ e_cnd s_thn s_els)]
  ;; break
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ break)
   break]
  ;; continue
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ continue)
   continue]
  ;; delete x
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (delete x))
   (delete x)]
  ;; delete attribute
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (delete (attribute e x)))
   (delete (attribute mode e- x))
   (where [mode e- T] (resolve-writable-attribute Ψ Γ_dcl Γ_lcl e x))]
  ;; ann x, they are ignored
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (ann x t))
   (begin)]
  ;; ann attribute, they are ignored
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (ann (attribute e x) t))
   (begin)]
  ;; ann-assign x, annotations are ignored
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (ann-assign x t e))
   (assign x (maybe-cast Ψ [e- T_src] T_dst))
   (where [e- T_src] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T_dcl (lookup Γ_dcl x))
   (where T_dst (intersection Ψ T_src T_dcl))]
  ;; ann-assign attribute, the annotation (t) is ignored
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (ann-assign (attribute e x) t e_src))
   (assign (attribute mode e- x) (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_src) T))
   (where [mode e- T] (resolve-writable-attribute Ψ Γ_dcl Γ_lcl e x))]
  ;; function-def
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (function-def x ([x_arg t_arg] ...) t_out level))
   (assign x e-)
   (where [e- T] (compile-function Ψ Γ_dcl ([x_arg t_arg] ...) t_out level))]
  ;; class
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (class x (e ...) (m ...)))
   (make-begin
    (assign x (class x (e-_prn ...)
                ([x_cmem (compile-check x_cmem T_cmem)] ...)
                ([x_imem (compile-check x_imem T_imem)] ...)))
    s-_cinit ...)
   (where (e-_prn ...) (maybe-add-object-e (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) ...))
   (where [([x_cmem T_cmem s-_cinit] ...)
           ([x_imem T_imem] ...)]
          (compile-m* Ψ Γ_dcl x m ...))]
  ;; import-from
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (import-from x_mod x_var))
   (import-from x_mod x_var)]
  ;; try
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (try s_bdy e_exn x_exn s_exn s_els))
   (try (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_bdy)
        (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_exn))
        x_exn
        (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_exn)
        (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_els))]
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (finally s_bdy s_fnl))
   (finally (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_bdy)
            (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_fnl))]
  ;; raise
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (raise e))
   (raise (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)))])
(define-metafunction SP-compiled
  compile-m* : Ψ Γ x m ... -> [([x_cmem T_cmem s-_cinit] ...)
                               ([x_imem T_imem] ...)]
  [(compile-m* Ψ Γ x m ...)
   [([x_cmem T_cmem s-_cinit] ...)
    ([x_imem T_imem] ...)]
   (where ([boolean x_mem T_mem s-_init] ...)
          ((compile-m Ψ Γ x m) ...))
   (where [([x_cmem T_cmem s-_cinit] ...)
           ([x_imem T_imem] ...)]
          (categorize-members [boolean x_mem T_mem s-_init] ...))])
(define-metafunction SP-compiled
  compile-mt* : Ψ Γ x m ... -> [([x_cmem T_cmem] ...)
                                ([x_imem T_imem] ...)]
  [(compile-mt* Ψ Γ x m ...)
   [([x_cmem T_cmem] ...)
    ([x_imem T_imem] ...)]
   (where ([boolean x_mem T_mem] ...)
          ((compile-mt Ψ Γ x m) ...))
   (where [([x_cmem T_cmem] ...)
           ([x_imem T_imem] ...)]
          (categorize-member-Ts [boolean x_mem T_mem] ...))])
(define-metafunction SP-compiled
  ;; categorize members into class level and instance level
  categorize-members : [boolean x T s-] ... -> [([x T s-] ...) ([x T] ...)]
  [(categorize-members) [() ()]]
  [(categorize-members [#t x T s-] any ...)
   [([x T s-] any_1 ...)
    (any_2 ...)]
   (where [(any_1 ...)
           (any_2 ...)]
          (categorize-members any ...))]
  [(categorize-members [#f x T s-] any ...)
   [(any_1 ...)
    ([x T] any_2 ...)]
   (where [(any_1 ...)
           (any_2 ...)]
          (categorize-members any ...))])
(define-metafunction SP-compiled
  ;; categorize members into class level and instance level
  categorize-member-Ts : [boolean x T] ... -> [([x T] ...) ([x T] ...)]
  [(categorize-member-Ts) [() ()]]
  [(categorize-member-Ts [#t x T] any ...)
   [([x T] any_1 ...)
    (any_2 ...)]
   (where [(any_1 ...)
           (any_2 ...)]
          (categorize-member-Ts any ...))]
  [(categorize-member-Ts [#f x T] any ...)
   [(any_1 ...)
    ([x T] any_2 ...)]
   (where [(any_1 ...)
           (any_2 ...)]
          (categorize-member-Ts any ...))])
(define-metafunction SP-compiled
  maybe-add-object-e : e- ... -> (e- ...)
  [(maybe-add-object-e) ((ref "object"))]
  [(maybe-add-object-e e- ...) (e- ...)])
(define-metafunction SP-compiled
  make-assert : e- -> s-
  [(make-assert e-)
   (if e- (begin) (expr (raise-error "assertion error")))])
(define-metafunction SP-compiled
  compile-m : Ψ Γ x m -> [boolean x T s-]
  ;; declare-only member can be either a ClassVar
  [(compile-m Ψ Γ x_cls (field x t))
   [#t
    x
    T
    (begin)]
   (where (ClassVar T) (eval-t Ψ Γ t))]
  ;; or an instance-level member
  [(compile-m Ψ Γ x_cls (field x t))
   [#f
    x
    T
    (begin)]
   (where T (eval-t Ψ Γ t))]
  ;; initialized members must all be class-level
  [(compile-m Ψ Γ x_cls (field x t e))
   [#t
    x
    T
    (assign (attribute fast x_cls x)
            (maybe-cast Ψ (compile-e Ψ Γ Γ e) T))]
   (where (ClassVar T) (eval-t Ψ Γ t))]
  ;; methods are ClassVar
  [(compile-m Ψ Γ x_cls (method x_mth ([x_slf dynamic] [x_arg t_arg] ...) t_out level))
   [#t
    x_mth
    T
    (assign (attribute fast x_cls x_mth) e-)]
   (where (Type T_cls) (lookup Γ x_cls))
   (where [e- T] (compile-method Ψ Γ x_slf T_cls ([x_arg t_arg] ...) t_out level))])
(define-metafunction SP-compiled
  compile-mt : Ψ Γ x m -> [boolean x T]
  ;; declare-only member can be either a ClassVar
  [(compile-mt Ψ Γ x_cls (field x t))
   [#t
    x
    T]
   (where (ClassVar T) (eval-t Ψ Γ t))]
  ;; or an instance-level member
  [(compile-mt Ψ Γ x_cls (field x t))
   [#f
    x
    T]
   (where T (eval-t Ψ Γ t))]
  ;; initialized members must all be class-level
  [(compile-mt Ψ Γ x_cls (field x t e))
   [#t
    x
    T]
   (where (ClassVar T) (eval-t Ψ Γ t))]
  ;; methods are ClassVar
  [(compile-mt Ψ Γ x_cls (method x_mth ([x_slf dynamic] [x_arg t_arg] ...) t_out level))
   [#t
    x_mth
    T]
   (where (Type T_cls) (lookup Γ x_cls))
   (where T (compile-method-T Ψ Γ x_slf T_cls ([x_arg t_arg] ...) t_out level))])
(define-metafunction SP-compiled
  compile-method : Ψ Γ x T ([x t] ...) t level -> [e- (-> (T ...) T)]
  [(compile-method Ψ Γ x_slf T_cls ([x_arg t_arg] ...) t_out level)
   [(lambda (x_slf x_arg ...)
      (make-begin
       (compile-check x_arg T_arg)
       ...)
      (compile-method-level Ψ Γ T_out x_slf T_cls level))
    (-> (T_arg ...) T_out)]
   (where (T_arg ...) ((eval-t Ψ Γ t_arg) ...))
   (where T_out (eval-t Ψ Γ t_out))])
(define-metafunction SP-compiled
  compile-method-T : Ψ Γ x T ([x t] ...) t level -> (-> (T ...) T)
  [(compile-method-T Ψ Γ x_slf T_cls ([x_arg t_arg] ...) t_out level)
   (-> (T_arg ...) T_out)
   (where (T_arg ...) ((eval-t Ψ Γ t_arg) ...))
   (where T_out (eval-t Ψ Γ t_out))])
(define-metafunction SP-compiled
  compile-method-level : Ψ Γ T x T level -> level-
  [(compile-method-level Ψ Γ T x_slf T_cls (local ([x d] ...) s))
   (local (x ...)
     (compile-s Ψ Γ_bdy Γ_bdy T s))
   (where #f (some-duplicates x ...))
   (where Γ_0 Γ)
   (where Γ_1 (extend Γ_0 [x dynamic] ...))
   (where Γ_2 (update Γ_1 [x (T-of-d Ψ Γ_1 d)] ...))
   (where Γ_3 (update Γ_2 [x_slf T_cls]))
   (where Γ_bdy Γ_3)])
(define-metafunction SP-compiled
  compile-function : Ψ Γ ([x t] ...) t level -> [e- (-> (T ...) T)]
  [(compile-function Ψ Γ ([x_arg t_arg] ...) t_out level)
   [(lambda (x_arg ...)
      (make-begin
       (compile-check x_arg T_arg)
       ...)
      (compile-level Ψ Γ T_out level))
    (-> (T_arg ...) T_out)]
   (where (T_arg ...) ((eval-t Ψ Γ t_arg) ...))
   (where T_out (eval-t Ψ Γ t_out))])
(define-metafunction SP-compiled
  compile-level : Ψ Γ T level -> level-
  [(compile-level Ψ Γ T (local ([x d] ...) s))
   (local (x ...)
     (compile-s Ψ Γ_bdy Γ_bdy T s))
   (where #f (some-duplicates x ...))
   (where Γ_0 Γ)
   (where Γ_1 (extend Γ_0 [x dynamic] ...))
   (where Γ_2 (update Γ_1 [x (T-of-d Ψ Γ_1 d)] ...))
   (where Γ_bdy Γ_2)])
(define-metafunction SP-compiled
  T-of-d : Ψ Γ d -> T
  ;; Assuming that we are in local scope, where classes are dynamic
  ;; The global scope should not use this.
  [(T-of-d Ψ Γ t)
   (eval-t Ψ Γ t)]
  [(T-of-d Ψ Γ (function-def (t_arg ...) t_out))
   (-> (T_arg ...) T_out)
   (where (T_arg ...) ((eval-t Ψ Γ t_arg) ...))
   (where T_out (eval-t Ψ Γ t_out))]
  [(T-of-d Ψ Γ (class (e ...) (m ...))) dynamic])
(define-metafunction SP-compiled
  compile-begin : Ψ Γ_dcl Γ_lcl T+☠ s ... -> s-
  [(compile-begin Ψ Γ_dcl Γ_lcl ☠)
   (begin)]
  [(compile-begin Ψ Γ_dcl Γ_lcl T)
   (begin)]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ s)
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ s)]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ (return e) s ...)
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ (return e))]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ (begin s_1 ...) s_2 ...)
   (compile-begin Ψ Γ_dcl Γ_lcl T+☠ s_1 ... s_2 ...)]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ (ann-assign x t e) s ...)
   (make-begin
    (assign x e-)
    (compile-begin Ψ Γ_dcl (update Γ_lcl [x T_lcl]) T+☠ (begin s ...)))
   (where T_dst (lookup Γ_dcl x))
   (where (e- T_src) (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T_lcl (intersection Ψ T_src T_dst))]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ (if e s_thn s_els) s ...)
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ (if e (begin s_thn s ...) (begin s_els s ...)))]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ (while e s_thn s_els) s ...)
   (begin
     (while e
            (compile-s Ψ Γ_dcl Γ_thn T+☠ s_thn)
            (compile-s Ψ Γ_dcl Γ_els T+☠ s_els))
     (compile-begin Ψ Γ_dcl Γ_els T+☠ s ...))
   (where [e- Γ_thn Γ_els] (condition Ψ Γ_dcl Γ_lcl e))]
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ s_1 s_2 ...)
   (make-begin (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_1)
               (compile-begin Ψ Γ_dcl Γ_lcl T+☠ s_2 ...))])
(define-metafunction SP-compiled
  condition : Ψ Γ Γ e -> [e- Γ Γ]
  [(condition Ψ Γ_dcl Γ_lcl (is x (con None)))
   [(is x (con None))
    Γ_thn
    Γ_els]
   (where T (lookup Γ_lcl x))
   (where Γ_thn (update Γ_lcl [x (intersection Ψ T (subof "NoneType"))]))
   (where Γ_els (update Γ_lcl [x (remove-None T)]))]
  [(condition Ψ Γ_dcl Γ_lcl (not e))
   [(if-exp e- (con #f) (con #t))
    Γ_els
    Γ_thn]
   (where [e- Γ_thn Γ_els] (condition Ψ Γ_dcl Γ_lcl e))]
  [(condition Ψ Γ_dcl Γ_lcl (and e_1 e_2))
   [(if-exp e-_1 e-_2 (con #f))
    Γ_11
    (union* Ψ Γ_12 Γ_2)]
   (where [e-_1 Γ_1 Γ_2] (condition Ψ Γ_dcl Γ_lcl e_1))
   (where [e-_2 Γ_11 Γ_12] (condition Ψ Γ_dcl Γ_1 e_2))]
  [(condition Ψ Γ_dcl Γ_lcl (or e_1 e_2))
   [(if-exp e-_1 (con #t) e-_2)
    (union* Ψ Γ_1 Γ_21)
    Γ_22]
   (where [e-_1 Γ_1 Γ_2] (condition Ψ Γ_dcl Γ_lcl e_1))
   (where [e-_2 Γ_21 Γ_22] (condition Ψ Γ_dcl Γ_2 e_2))]
  [(condition Ψ Γ_dcl Γ_lcl e)
   [(as-dyn (compile-e Ψ Γ_dcl Γ_lcl e))
    Γ_lcl
    Γ_lcl]])
(define-metafunction SP-compiled
  union* : Ψ Γ Γ -> Γ
  [(union* Ψ ([x T_1] ...) ([x T_2] ...))
   ([x (union Ψ T_1 T_2)] ...)])
(define-metafunction SP-compiled
  compile-s-if : Ψ Γ Γ T+☠ e s s -> s-
  ;; base case
  [(compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out e s_thn s_els)
   (if (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) s-_thn s-_els)
   (where [e- Γ_thn Γ_els] (condition Ψ Γ_dcl Γ_lcl e))
   (where s-_thn (compile-s Ψ Γ_dcl Γ_thn T+☠_out s_thn))
   (where s-_els (compile-s Ψ Γ_dcl Γ_els T+☠_out s_els))])
(define-metafunction SP-compiled
  compile-s-while : Ψ Γ Γ T+☠ e s s -> s-
  ;; base case
  [(compile-s-while Ψ Γ_dcl Γ_lcl T+☠_out e s_thn s_els)
   (while (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) s-_thn s-_els)
   (where [e- Γ_thn Γ_els] (condition Ψ Γ_dcl Γ_lcl e))
   (where s-_thn (compile-s Ψ Γ_dcl Γ_thn T+☠_out s_thn))
   (where s-_els (compile-s Ψ Γ_dcl Γ_els T+☠_out s_els))])

(module+ test
  (test-equal (term (compile-program
                     (desugar-program
                      ((import-from "__static__" ("CheckedDict"))
                       (ann-assign "d" (subscript "CheckedDict" (tuple ("str" "int")))
                                   (call (subscript "CheckedDict" (tuple ("str" "int")))
                                         ((dict ([(con "foo") (con 123)])))))))))
              (term (local
                      ("CheckedDict" "d")
                      (begin
                        (import-from "__static__" "CheckedDict")
                        (assign
                         "d"
                         (new (chkdict (subof "str") (subof "int"))
                              ((dict (((ref (con "foo")) (ref (con 123))))))))))))
  (test-equal (term (compile-program
                     (desugar-program
                      ((import-from "__static__" ("CheckedDict"))
                       (class "C" ((subscript "CheckedDict" (tuple ("str" "int"))))
                         (pass))
                       (class "D" ("C")
                         (pass))))))
              (term (local
                      ("CheckedDict" "C" "D")
                      (begin
                        (import-from "__static__" "CheckedDict")
                        (assign "C"
                                (class "C"
                                  ((invoke-function
                                    (method "CheckedDict[_,_]" "__getitem__")
                                    ("CheckedDict"
                                     (tuple ("str" "int")))))
                                  ()
                                  ()))
                        (assign "D"
                                (class "D"
                                  ("C")
                                  ()
                                  ())))))))
(define-metafunction SP-compiled
  compile-program : program -> program-
  [(compile-program (local ([x d] ...) s))
   (local (x ...)
     (compile-s Ψ_global Γ_global Γ_global ☠ s))
   (where #f (some-duplicates x ...))
   (where ([x_imp d_imp] ...) (gather-import [x d] ...))
   (where ([x_cls d_cls] ...) (gather-class [x d] ...))
   (where ([x_oth d_oth] ...) (gather-other [x d] ...))
   ;; define classes abstractly
   (where Ψ_global-1 (extend (base-Ψ)
                             [(user-defined-class x_cls) ☠]
                             ...))
   ;; define classes and imports
   (where Γ_global-1 (extend (base-Γ)
                             [x_imp (T-of-import d_imp)]
                             ...
                             [x_cls (Type (subof (user-defined-class x_cls)))]
                             ...
                             [x_oth ☠]
                             ...))
   ;; define variables
   (where Γ_global-2 (update Γ_global-1
                             [x_oth (T-of-d Ψ_global-1 Γ_global-1 d_oth)]
                             ...))
   ;; define classes concretely
   (where Ψ_global-2 (update Ψ_global-1
                             [(user-defined-class x_cls)
                              (eval-class-d Ψ_global-1 Γ_global-2 x_cls d_cls)]
                             ...))
   ;(judgment-holds (show (Γ_global-2 Ψ_global-2)))
   (judgment-holds (⊢Ψ Ψ_global-2))
   (where Γ_global Γ_global-2)
   (where Ψ_global Ψ_global-2)])
(define-metafunction SP-compiled
  some-duplicates : x ... -> boolean
  [(some-duplicates x_0 ... x x_1 ... x x_2 ...) #t]
  [(some-duplicates x ...) #f])
(define-metafunction SP-compiled
  eval-class-d : Ψ Γ x (class (e ...) (m ...)) -> (class l*+dynamic Γ ρ Γ)
  [(eval-class-d Ψ Γ x (class (e ...) (m ...)))
   (class (l_1 ...) Γ_cls ρ_cls Γ_ins)
   (where ((subof l_0) ...) ((eval-t Ψ Γ e) ...))
   (where (l_1 ...) (maybe-add-object l_0 ...))
   (where [([x_cmem T_cmem] ...)
           ([x_imem T_imem] ...)]
          (compile-mt* Ψ Γ x m ...))
   (where Γ_cls ([x_cmem T_cmem] ...))
   (where ρ_cls ([x_cmem (method (user-defined-class x) x_cmem)] ...))
   (where Γ_ins ([x_imem T_imem] ...))]
  [(eval-class-d Ψ Γ x (class (e ...) (m ...)))
   (class dynamic Γ_cls ρ_cls Γ_ins)
   (where [([x_cmem T_cmem] ...)
           ([x_imem T_imem] ...)]
          (compile-mt* Ψ Γ x m ...))
   (where Γ_cls ([x_cmem T_cmem] ...))
   (where ρ_cls ([x_cmem (method (user-defined-class x) x_cmem)] ...))
   (where Γ_ins ([x_imem T_imem] ...))])
(define-metafunction SP-compiled
  maybe-add-object : l ... -> (l ...)
  [(maybe-add-object)
   ("object")]
  [(maybe-add-object l_0 l_1 ...)
   (l_0 l_1 ...)])
(define-metafunction SP-compiled
  T-of-import : (import-from x x) -> T
  [(T-of-import (import-from "__static__" "CheckedDict"))
   (Type (subof "CheckedDict[_,_]"))]
  [(T-of-import (import-from "__static__" "PyDict"))
   (Type (subof "dict"))]
  [(T-of-import (import-from "__static__" "pydict"))
   (Type (subof "dict"))]
  [(T-of-import (import-from "__static__" "cast"))
   "cast"]
  [(T-of-import (import-from "__static__" "inline"))
   dynamic]
  [(T-of-import (import-from "typing" "Optional"))
   (Type (subof "Optional[_]"))]
  [(T-of-import (import-from "typing" "Any"))
   (Type dynamic)]
  [(T-of-import (import-from "typing" "Final"))
   (Type (subof "Final[_]"))]
  [(T-of-import (import-from "typing" "ClassVar"))
   (Type (subof "ClassVar[_]"))]
  [(T-of-import (import-from "typing" "List"))
   (Type (subof "list"))]
  [(T-of-import (import-from "__future__" "annotations"))
   dynamic]
  ;; fallback to dynamic
  [(T-of-import (import-from x_mod x_var))
   dynamic
   (where #f ,(redex-match? SP-compiled "__static__" (term x_mod)))
   (where #f ,(redex-match? SP-compiled "typing" (term x_mod)))])
(define-metafunction SP-compiled
  gather-import : [x d] ... -> ([x (import-from x x)] ...)
  [(gather-import)
   ()]
  [(gather-import [x_1 import-d] [x_2 d_2] ...)
   (append ([x_1 import-d]) (gather-import [x_2 d_2] ...))]
  [(gather-import [x_1 d_1] [x_2 d_2] ...)
   (gather-import [x_2 d_2] ...)])
(define-metafunction SP-compiled
  gather-class : [x d] ... -> ([x (class (e ...) (m ...))] ...)
  [(gather-class)
   ()]
  [(gather-class [x_1 (class (e ...) (m ...))] [x_2 d_2] ...)
   (append ([x_1 (class (e ...) (m ...))]) (gather-class [x_2 d_2] ...))]
  [(gather-class [x_1 d_1] [x_2 d_2] ...)
   (gather-class [x_2 d_2] ...)])
(define-metafunction SP-compiled
  gather-other : [x d] ... -> ([x d]...)
  [(gather-other)
   ()]
  [(gather-other [x_1 other-d] [x_2 d_2] ...)
   (append ([x_1 other-d]) (gather-other [x_2 d_2] ...))]
  [(gather-other [x_1 d_1] [x_2 d_2] ...)
   (gather-other [x_2 d_2] ...)])

