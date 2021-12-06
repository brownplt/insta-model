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
      ;;   the s- is the check to be perform
      (lambda (x ...) s- level-)
      (class
          ;; parent classes
          (e- ...)
        ;; members and corresponding checks
        ;;   when no check is present, the member is immutable
        ([x s-] ...))
      ;; new construct
      (raise-error)
      )

  ;; statements
  (s- (expr e-)
      (return e-)
      (begin s- ...)
      (if e- s- s-)
      (delete x)
      (delete (attribute mode e- x))
      ;; annotations are removed!
      (assign x e-)
      (assign (attribute mode e- x) e-)
      (import-from x x))

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

  (C (class l*+dynamic Γ ρ))
  (C+☠ C ☠)
  (l*+dynamic
   (l ...)
   ;; subclass from a dynamic or unsupported class
   dynamic)

  ;; a local environment that maps variables to their types
  (Γ ([x T+☠] ...))

  ;; a local environment that maps names to labels
  (ρ ([x l] ...))

  ;; types
  (T dynamic
     (exactness class-l)
     (Optional nonnonable-T)
     (-> (T ...) T)
     ;; The remaining are not really types.
     ;;   Each of them only has one inhabitant
     (Type T)
     type-op
     cast)

  ;; maybe type
  (T+☠ T ☠)

  ;; type-op
  (type-op "CheckedDict[_,_]"
           "Optional[_]"
           "Final[_]"
           (Union class-l))

  (exactness exact subof)

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
   "CheckedDict[_,_]"
   (tuple (checkable-T ...))
   (chkdict checkable-T checkable-T)))

(define-metafunction SP-compiled
  let : ([x e-] ...) s- -> e-
  [(let ([x e-] ...) s-)
   (call-function (lambda (x ...) (begin) (local (x ...) s-))
                  (e- ...))])

(define-metafunction SP-compiled
  lookup-Ψ : Ψ class-l -> (class l*+dynamic Γ ρ)
  ;; Find the meaning of a class by its cid
  ;; lookup user-defined classes
  [(lookup-Ψ Ψ (user-defined-class x))
   (lookup Ψ (user-defined-class x))]
  ;; primitive/builtin classes are handled here
  [(lookup-Ψ Ψ "CheckedDict[_,_]")
   (class ("object")
     (["__getitem__" "CheckedDict[_,_]"])
     (["__getitem__" (method "CheckedDict[_,_]" "__getitem__")]))]
  [(lookup-Ψ Ψ "Optional[_]")
   (class ("object")
     (["__getitem__" "Optional[_]"])
     (["__getitem__" (method "Optional[_]" "__getitem__")]))]
  [(lookup-Ψ Ψ "object")
   (class ()
     (["__init__" (-> (dynamic) dynamic)])
     (["__init__" (method "object" "__init__")]))]
  [(lookup-Ψ Ψ "type")
   (class ("object")
     ()
     ())]
  [(lookup-Ψ Ψ "int")
   (class ("object")
     (["__init__" (-> (dynamic dynamic) dynamic)]
      ["__gt__" (-> (dynamic (subof "int")) (subof "bool"))]
      ["__lt__" (-> (dynamic (subof "int")) (subof "bool"))]
      ["__eq__" (-> (dynamic (subof "int")) (subof "bool"))]
      ["__le__" (-> (dynamic (subof "int")) (subof "bool"))]
      ["__ge__" (-> (dynamic (subof "int")) (subof "bool"))]
      ["__neg__" (-> (dynamic) (subof "bool"))]
      ["__add__" (-> (dynamic (subof "int")) (subof "int"))]
      ["__sub__" (-> (dynamic (subof "int")) (subof "int"))]
      ["__mul__" (-> (dynamic (subof "int")) (subof "int"))]
      ["__div__" (-> (dynamic (subof "int")) (subof "int"))])
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
      ["__div__" (method "int" "__div__")]))]
  [(lookup-Ψ Ψ "bool")
   (class ("int") () ())]
  [(lookup-Ψ Ψ "str")
   (class ("object")
     (["__init__" (-> (dynamic dynamic) dynamic)])
     (["__init__" (method "str" "__init__")]))]
  [(lookup-Ψ Ψ "dict")
   (class ("object")
     (["__init__" (-> (dynamic dynamic) dynamic)]
      ["__getitem__" (-> (dynamic dynamic) dynamic)]
      ["__setitem__" (-> (dynamic dynamic dynamic) (subof "NoneType"))]
      ["__delitem__" (-> (dynamic dynamic) (subof "NoneType"))])
     (["__init__" (method "dict" "__init__")]
      ["__getitem__" (method "dict" "__getitem__")]
      ["__setitem__" (method "dict" "__setitem__")]
      ["__delitem__" (method "dict" "__delitem__")]))]
  [(lookup-Ψ Ψ "set")
   (class ("object")
     (["__init__" (-> (dynamic dynamic) dynamic)]
      ["__contains__" (-> (dynamic dynamic) (subof "bool"))])
     (["__init__" (method "set" "__init__")]
      ["__contains__" (method "set" "__contains__")]))]
  [(lookup-Ψ Ψ "list")
   (class ("object")
     (["__init__" (-> (dynamic dynamic) dynamic)])
     (["__init__" (method "list" "__init__")]))]
  [(lookup-Ψ Ψ "NoneType")
   (class ("object") () ())]
  [(lookup-Ψ Ψ (chkdict T_key T_val))
   (class ("object")
     (["__init__" (-> (dynamic dynamic) dynamic)]
      ["get" (-> (T_key) (union Ψ (subof "NoneType") T_val))]
      ["keys" (-> () (subof "list"))]
      ["__getitem__" (-> (dynamic T_key) T_val)]
      ["__setitem__" (-> (dynamic T_key T_val) (subof "NoneType"))]
      ["__delitem__" (-> (dynamic T_key) (subof "NoneType"))])
     (["__init__" (method (chkdict T_key T_val) "__init__")]
      ["get" (method (chkdict T_key T_val) "get")]
      ["keys" (method (chkdict T_key T_val) "keys")]
      ["__getitem__" (method (chkdict T_key T_val) "__getitem__")]
      ["__setitem__" (method (chkdict T_key T_val) "__setitem__")]
      ["__delitem__" (method (chkdict T_key T_val) "__delitem__")]))])

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

(module+ test
  (check-judgment-holds*
   (Ψ⊢class-l<:class-l () "NoneType" "object")
   (Ψ⊢class-l<:class-l () "NoneType" "NoneType"))
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

  [(where (class (l_sup ...) Γ ρ) (lookup-Ψ Ψ l_lft))
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

(define-metafunction SP-compiled
  intersection : Ψ T T -> T+☠
  [(intersection Ψ T_1 T_2)
   T_1
   (judgment-holds (Ψ⊢T<:T Ψ T_1 T_2))]
  [(intersection Ψ T_1 T_2)
   T_2
   (judgment-holds (Ψ⊢T<:T Ψ T_2 T_1))]
  [(intersection Ψ (Optional T_1) (Optional T_2))
   (subof "NoneType")]
  [(intersection Ψ T_1 T_2) ☠])

(define-metafunction SP-compiled
  remove-None : T -> T
  [(remove-None (Optional T)) T]
  ;[(remove-None (subof "NoneType")) dynamic]  ;; If we want to be pedantic, should be bottom
  [(remove-None T) T])

(module+ test
  (test-equal (term (lookup-method-T (base-Ψ) "bool" "__add__"))
              (term (-> (dynamic (subof "int")) (subof "int")))))
(define-metafunction SP-compiled
  lookup-method-T : Ψ l x -> T
  ;; a hack to support __or__ union type
  [(lookup-method-T Ψ l_cls "__or__")
   (Union l_cls)]
  ;; if in the current class, return the type
  [(lookup-method-T Ψ l_cls x)
   T
   (where (class (l_sup ...) Γ ρ) (lookup-Ψ Ψ l_cls))
   (where T (lookup (append Γ ([x ☠])) x))]
  ;; if there is only one parent, go to that parent
  [(lookup-method-T Ψ l_cls x)
   (lookup-method-T Ψ l_sup x)
   (where (class (l_sup) Γ ρ) (lookup-Ψ Ψ l_cls))]
  ;; fall back
  [(lookup-method-T Ψ l_cls x)
   dynamic])

(define-metafunction SP-compiled
  lookup-method-l : Ψ l x -> l
  ;; a hack to support __or__ union type
  [(lookup-method-l Ψ l_cls "__or__")
   (method l_cls "__or__")]
  ;; if in the current class, return the type
  [(lookup-method-l Ψ l_cls x)
   l
   (where (class (l_sup ...) Γ ρ) (lookup-Ψ Ψ l_cls))
   (where l (lookup (append ρ ([x ☠])) x))]
  ;; if there is only one parent, go to that parent
  [(lookup-method-l Ψ l_cls x)
   (lookup-method-l Ψ l_sup x)
   (where (class (l_sup) Γ ρ) (lookup-Ψ Ψ l_cls))])


(module+ test
  (test-equal (term (eval-t (base-Ψ) (prim-Γ) (prim-Γ) dynamic))
              (term dynamic))
  (test-equal (term (eval-t (base-Ψ) (prim-Γ) (prim-Γ) "int"))
              (term (subof "int")))
  (test-equal (term (eval-t (base-Ψ) (prim-Γ) (prim-Γ)
                            (desugar-e (subscript "CheckedDict" (tuple ("int" "str"))))))
              (term (subof (chkdict (subof "int") (subof "str"))))))
(define-metafunction SP-compiled
  eval-t : Ψ Γ Γ t -> T
  ;; the dynamic cases
  [(eval-t Ψ Γ_dcl Γ_lcl dynamic)
   dynamic]
  ;; If we got an type object, we know it represents the type
  ;; special case
  [(eval-t Ψ Γ_dcl Γ_lcl (con x))
   (eval-t Ψ Γ_dcl Γ_lcl x)]
  ;; normal case
  [(eval-t Ψ Γ_dcl Γ_lcl e)
   (T-of-T T)
   (where [e- T] (compile-e Ψ Γ_dcl Γ_lcl e))])
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
              (term [(call-function "int" ("bool"))
                     dynamic]))
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
   (compile-e Ψ Γ_dcl Γ_lcl (call (lambda ([x_tmp dynamic]) (if-exp x_tmp e_2 x_tmp)) (e_1)))
   (where x_tmp ,(symbol->string (gensym 'and)))]
  ;; or
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
       (return "tmp")))])
(define-metafunction SP-compiled
  compile-e-lambda : Ψ Γ Γ ([x t] ...) e -> [e- T]
  [(compile-e-lambda Ψ Γ_dcl Γ_lcl ([x_arg t_arg] ...) e_out)
   [(lambda (x_arg ...)
      (make-begin (compile-check x_arg T_arg) ...)
      (local (x_arg ...) (return e-_out)))
    (-> (T_arg ...) T_out)]
   (where (T_arg ...) ((eval-t Ψ Γ_dcl Γ_lcl t_arg) ...))
   (where [e-_out T_out] (compile-e Ψ Γ_dcl (extend Γ_dcl [x_arg T_arg] ...) e_out))])
(module+ test
  (test-equal (term (type-call (base-Ψ) "CheckedDict[_,_]" (exact (tuple ((Type (subof "int"))
                                                                 (Type (subof "str")))))))
              (term (Type (subof (chkdict (subof "int") (subof "str"))))))
  (test-equal (term (type-call (base-Ψ) "Optional[_]" (Type (exact "int"))))
              (term (Type (Optional (exact "int"))))))
(define-metafunction SP-compiled
  type-call : Ψ type-op T -> T
  [(type-call Ψ (Union l_1) T)
   (Type (union (base-Ψ) (subof l_1) (T-of-T T)))]
  [(type-call Ψ "CheckedDict[_,_]" (exact (tuple (T_key T_val))))
   (Type (subof (chkdict (T-of-T T_key) (T-of-T T_val))))]
  [(type-call Ψ "Optional[_]" T)
   (Type (Optional (T-of-T T)))])
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
   (where type-op (lookup-method-T Ψ l_cls x_mth))
   (where l_mth (lookup-method-l Ψ l_cls x_mth))
   (where ([e-_arg T_arg] ...) ((compile-e Ψ Γ_dcl Γ_lcl e_arg) ...))]
  ;; we emit INVOKE_FUNCTION when the exact class is known
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   [(invoke-function l_mth (e-_obj (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where [e-_obj (exact l_cls)] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where (-> (T_obj T_arg ...) T_out) (lookup-method-T Ψ l_cls x_mth))
   (where l_mth (lookup-method-l Ψ l_cls x_mth))]
  ;; we emit INVOKE_METHOD when the class is known but inexact
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   [(invoke-method l_cls x_mth e-_obj ((maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where [e-_obj (subof l_cls)] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where (-> (dynamic T_arg ...) T_out) (lookup-method-T Ψ l_cls x_mth))]
  ;; fall back
  [(compile-e-call Ψ Γ_dcl Γ_lcl e_fun (e_arg ...))
   [(call-function (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_fun))
                   ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_arg)) ...))
    dynamic]])
(define-metafunction SP-compiled
  compile-e-attribute : Ψ Γ Γ e x -> [e- T]
  [(compile-e-attribute Ψ Γ_dcl Γ_lcl e x)
   [(attribute mode e- x)
    T]
   (where [mode e- T] (resolve-attribute Ψ Γ_dcl Γ_lcl e x))]
  ; [(compile-e-attribute Ψ Γ_dcl Γ_lcl e x)
  ;  [(ref l_mth)
  ;   (-> ((subof l_cls) T_arg ...) T_out)]
  ;  (where [e- (Type (subof l_cls))] (compile-e Ψ Γ_dcl Γ_lcl e))
  ;  (where (-> (T_arg ...) T_out) (lookup-method-T Ψ l_cls x))
  ;  (where l_mth (lookup-method-l Ψ l_cls x))]
  )
(define-metafunction SP-compiled
  resolve-attribute : Ψ Γ Γ e x -> [mode e- T]
  [(resolve-attribute Ψ Γ_dcl Γ_lcl e x)
   [fast e- T]
   (where [e- (exactness l)] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T (lookup-method-T Ψ l x))]
  [(resolve-attribute Ψ Γ_dcl Γ_lcl e x)
   [safe (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) dynamic]])
(define-metafunction SP-compiled
  compile-e-if-exp : Ψ Γ Γ e e e -> [e- T]
  ;; base case
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl (is x None) e_thn e_els)
   [(if-exp (as-dyn (compile-e (is x None))) e-_thn e-_els)
    (union Ψ T_thn T_els)]
   (where T (lookup Γ_lcl x))
   (where Γ_thn (update Γ_lcl [x (intersection Ψ T (subof "NoneType"))]))
   (where Γ_els (update Γ_lcl [x (remove-None  Ψ T (subof "NoneType"))]))
   (where [e-_thn T_thn] (compile-e Ψ Γ_dcl Γ_thn e_thn))
   (where [e-_els T_els] (compile-e Ψ Γ_dcl Γ_els e_els))]
  ;; step case -- not
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl (not e) e_thn e_els)
   (compile-e-if-exp Ψ Γ_dcl Γ_lcl e e_thn e_els)]
  ;; step case -- and
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl (and e_1 e_2) e_thn e_els)
   (compile-e-if-exp Ψ Γ_dcl Γ_lcl e_1 (if-exp e_2 e_thn e_els) e_els)]
  ;; step case -- or
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl (or e_1 e_2) e_thn e_els)
   (compile-e-if-exp Ψ Γ_dcl Γ_lcl e_1 e_thn (if-exp e_2 e_thn e_els))]
  ;; fall through
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl e e_thn e_els)
   [(if-exp (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) e-_thn e-_els)
    (union Ψ T_thn T_els)]
   (where [e-_thn T_thn] (compile-e Ψ Γ_dcl Γ_lcl e_thn))
   (where [e-_els T_els] (compile-e Ψ Γ_dcl Γ_lcl e_els))])

(define-metafunction SP-compiled
  as-dyn : [e- T] -> e-
  [(as-dyn [e- T]) e-])

(define-metafunction SP-compiled
  compile-check : e- checkable-T -> s-
  [(compile-check e- dynamic) (begin)]
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
              (term (if (ref (con #t)) (begin) (expr (raise-error)))))
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
  ;; delete attribute
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (delete (attribute (con 2) "__add__"))))
              (term (delete (attribute fast (ref (con 2)) "__add__"))))
  ;; ann x
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (ann "i" "int")))
              (term (begin)))
  ;; ann attribute
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (ann (attribute (con 2) "__add__") "int")))
              (term (begin)))
  ;; ann-assign
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
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
                                        (expr (raise-error))))
                                  (local ("i")
                                    (return "i"))))))
  ;; class
  (test-equal (term (compile-s (base-Ψ) (prim-Γ) (prim-Γ) ☠
                               (class "C" ("dict")
                                 ((field "a" "int" (con 2))
                                  (field "b" dynamic)
                                  (method "f" (["self" dynamic] ["i" "int"]) "int"
                                          (local (["self" dynamic] ["i" "int"])
                                            (return "i")))))))
              (term (begin
                      (assign "C" (class ("dict")
                                    (["a" (if (invoke-function "issubclass"
                                                               ((invoke-function "type" ("a"))
                                                                (ref "int")))
                                              (begin)
                                              (expr (raise-error)))]
                                     ["b" (begin)]
                                     ["f" (expr (raise-error))])))
                      (assign (attribute fast "C" "a") (ref (con 2)))
                      (assign (attribute fast "C" "f")
                              (lambda ("self" "i")
                                (begin
                                  (if (invoke-function "issubclass"
                                                       ((invoke-function "type" ("i"))
                                                        (ref "int")))
                                      (begin)
                                      (expr (raise-error))))
                                (local ("self" "i")
                                  (return "i")))))))
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
  ;; delete x
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (delete x))
   (delete x)]
  ;; delete attribute
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (delete (attribute e x)))
   (delete (attribute mode e- x))
   (where [mode e- T] (resolve-attribute Ψ Γ_dcl Γ_lcl e x))]
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
   (where [mode e- T] (resolve-attribute Ψ Γ_dcl Γ_lcl e x))]
  ;; function-def
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (function-def x ([x_arg t_arg] ...) t_out level))
   (assign x e-)
   (where [e- T] (compile-function Ψ Γ_dcl ([x_arg t_arg] ...) t_out level))]
  ;; class
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (class x (e ...) (m ...)))
   (make-begin
    (assign x (class ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
                ([x_mem s-_check] ...)))
    s-_init ...)
   (where ([x_mem s-_check s-_init] ...)
          ((compile-m Ψ Γ_dcl Γ_lcl x m) ...))]
  ;; import-from
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (import-from x_mod x_var))
   (import-from x_mod x_var)])
(define-metafunction SP-compiled
  make-assert : e- -> s-
  [(make-assert e-)
   (if e- (begin) (expr (raise-error)))])
(define-metafunction SP-compiled
  compile-m : Ψ Γ Γ x m -> [x s- s-]
  [(compile-m Ψ Γ_dcl Γ_lcl x_cls (field x t))
   [x
    (compile-check x T)
    (begin)]
   (where T (eval-t Ψ Γ_dcl Γ_lcl t))]
  [(compile-m Ψ Γ_dcl Γ_lcl x_cls (field x t e))
   [x
    (compile-check x T)
    (assign (attribute fast x_cls x)
            (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e) T))]
   (where T (eval-t Ψ Γ_dcl Γ_lcl t))]
  [(compile-m Ψ Γ_dcl Γ_lcl x_cls (method x ([x_arg t_arg] ...) t_out level))
   [x
    (expr (raise-error)) ;; TODO: let's assume methods are immutable right now
    (assign (attribute fast x_cls x) e-)]
   (where [e- T]
          (compile-function Ψ Γ_dcl ([x_arg t_arg] ...) t_out level))])
(define-metafunction SP-compiled
  compile-function : Ψ Γ ([x t] ...) t level -> [e- (-> (T ...) T)]
  [(compile-function Ψ Γ ([x_arg t_arg] ...) t_out level)
   [(lambda (x_arg ...)
      (make-begin
       (compile-check x_arg T_arg)
       ...)
      (compile-level Ψ Γ T_out level))
    (-> (T_arg ...) T_out)]
   (where (T_arg ...) ((eval-t Ψ Γ Γ t_arg) ...))
   (where T_out (eval-t Ψ Γ Γ t_out))])
(define-metafunction SP-compiled
  compile-level : Ψ Γ T level -> level-
  [(compile-level Ψ Γ T (local ([x d] ...) s))
   (local (x ...)
     (compile-s Ψ Γ_bdy Γ_bdy T s))
   (where Γ_0 Γ)
   (where Γ_1 (extend Γ_0 [x dynamic] ...))
   (where Γ_2 (update Γ_1 [x (T-of-d Ψ Γ_1 d)] ...))
   (where Γ_bdy Γ_2)])
(define-metafunction SP-compiled
  T-of-d : Ψ Γ d -> T
  ;; Assuming that we are in local scope, where classes are dynamic
  ;; The global scope should not use this.
  [(T-of-d Ψ Γ t)
   (eval-t Ψ Γ Γ t)]
  [(T-of-d Ψ Γ (function-def (t_arg ...) t_out))
   (-> (T_arg ...) T_out)
   (where (T_arg ...) ((eval-t Ψ Γ Γ t_arg) ...))
   (where T_out (eval-t Ψ Γ Γ t_out))]
  [(T-of-d Ψ Γ (class (e ...) ([x d] ...))) dynamic])
(define-metafunction SP-compiled
  compile-begin : Ψ Γ_dcl Γ_lcl T+☠ s ... -> s-
  [(compile-begin Ψ Γ_dcl Γ_lcl ☠)
   (begin)]
  [(compile-begin Ψ Γ_dcl Γ_lcl T)
   (compile-s Ψ Γ_dcl Γ_lcl T (return (con None)))]
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
  [(compile-begin Ψ Γ_dcl Γ_lcl T+☠ s_1 s_2 ...)
   (make-begin (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_1)
               (compile-begin Ψ Γ_dcl Γ_lcl T+☠ s_2 ...))])
(define-metafunction SP-compiled
  compile-s-if : Ψ Γ Γ T+☠ e s s -> s-
  ;; base case
  [(compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out (is x None) s_thn s_els)
   (if (as-dyn (compile-e (is x None))) s-_thn s-_els)
   (where T (lookup Γ_lcl x))
   (where Γ_thn (update Γ_lcl [x (intersection Ψ T (subof "NoneType"))]))
   (where Γ_els (update Γ_lcl [x (remove-None  Ψ T (subof "NoneType"))]))
   (where s-_thn (compile-s Ψ Γ_dcl Γ_thn T+☠_out s_thn))
   (where s-_els (compile-s Ψ Γ_dcl Γ_els T+☠_out s_els))]
  ;; step case -- not
  [(compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out (not e) s_thn s_els)
   (compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out e s_thn s_els)]
  ;; step case -- and
  [(compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out (and e_1 e_2) s_thn s_els)
   (compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out e_1 (if e_2 s_thn s_els) s_els)]
  ;; step case -- or
  [(compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out (or e_1 e_2) s_thn s_els)
   (compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out e_1 s_thn (if e_2 s_thn s_els))]
  ;; fall through
  [(compile-s-if Ψ Γ_dcl Γ_lcl T+☠_out e s_thn s_els)
   (if (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) s-_thn s-_els)
   (where s-_thn (compile-s Ψ Γ_dcl Γ_lcl T+☠_out s_thn))
   (where s-_els (compile-s Ψ Γ_dcl Γ_lcl T+☠_out s_els))])

(module+ test
  (test-equal (term (compile-program
                     (desugar-program
                      ((import-from "__static__" ("CheckedDict"))
                       (ann-assign "d" (subscript "CheckedDict" (tuple ("str" "int")))
                                   (call (subscript "CheckedDict" (tuple ("str" "int")))
                                         ((dict ([(con "foo") (con "123")])))))))))
              ;; TODO: This isn't ideal
              (term (local
                      ("CheckedDict" "d")
                      (begin
                        (import-from "__static__" "CheckedDict")
                        (assign
                         "d"
                         (call-function
                          (invoke-function
                           (method "CheckedDict[_,_]" "__getitem__")
                           ("CheckedDict" (tuple ("str" "int"))))
                          ((dict
                            (((ref (con "foo")) (ref (con "123"))))))))))))
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
                        (assign
                         "C"
                         (class ((invoke-function
                                  (method "CheckedDict[_,_]" "__getitem__")
                                  ("CheckedDict"
                                   (tuple ("str" "int")))))
                           ()))
                        (assign "D" (class ("C") ())))))))
(define-metafunction SP-compiled
  compile-program : program -> program-
  [(compile-program (local ([x d] ...) s))
   (local (x ...)
     (compile-s Ψ_global Γ_global Γ_global ☠ s))
   (where ([x_imp d_imp] ...) (gather-import [x d] ...))
   (where ([x_cls d_cls] ...) (gather-class [x d] ...))
   (where ([x_oth d_oth] ...) (gather-other [x d] ...))
   (where Ψ_global-1 (extend (base-Ψ)
                             [(user-defined-class x_cls) ☠]
                             ...))
   (where Γ_global-1 (extend (base-Γ)
                             [x_imp (T-of-import d_imp)]
                             ...
                             [x_cls (Type (subof (user-defined-class x_cls)))]
                             ...
                             [x_oth ☠]
                             ...))
   (where Γ_global-2 (update Γ_global-1
                             [x_oth (T-of-d Ψ_global-1 Γ_global-1 d_oth)]
                             ...))
   (where Ψ_global-2 (update Ψ_global-1
                             [(user-defined-class x_cls)
                              (eval-class-d Ψ_global-1 Γ_global-2 x_cls d_cls)]
                             ...))
   (where Γ_global Γ_global-2)
   (where Ψ_global Ψ_global-2)])
(define-metafunction SP-compiled
  eval-class-d : Ψ Γ x (class (e ...) ([x_mem d_mem] ...)) -> (class l*+dynamic Γ ρ)
  [(eval-class-d Ψ Γ x (class (e ...) ([x_mem d_mem] ...)))
   (class (l ...) Γ_cls ρ_cls)
   (where ((subof l) ...) ((eval-t Ψ Γ Γ e) ...))
   (where Γ_cls ([x_mem (T-of-d Ψ Γ d_mem)] ...))
   (where ρ_cls ([x_mem (method (user-defined-class x) x_mem)] ...))]
  [(eval-class-d Ψ Γ x (class (e ...) ([x_mem d_mem] ...)))
   (class dynamic Γ_cls ρ_cls)
   (where Γ_cls ([x_mem (T-of-d Ψ Γ d_mem)] ...))
   (where ρ_cls ([x_mem (method (user-defined-class x) x_mem)] ...))])
(define-metafunction SP-compiled
  T-of-import : (import-from x x) -> T
  [(T-of-import (import-from "__static__" "CheckedDict"))
   (Type (subof "CheckedDict[_,_]"))]
  [(T-of-import (import-from "__static__" "PyDict"))
   (Type (subof "dict"))]
  [(T-of-import (import-from "__static__" "pydict"))
   (Type (subof "dict"))]
  ;; TODO
  [(T-of-import (import-from "__static__" "inline"))
   dynamic]
  [(T-of-import (import-from "typing" "Optional"))
   (Type (subof "Optional[_]"))]
  [(T-of-import (import-from "typing" "Any"))
   (Type dynamic)]
  ;; TODO
  [(T-of-import (import-from "typing" "Final"))
   (Type (subof "Final[_]"))]
  [(T-of-import (import-from "__future__" "annotations"))
   dynamic])
(define-metafunction SP-compiled
  gather-import : [x d] ... -> ([x (import-from x x)] ...)
  [(gather-import)
   ()]
  [(gather-import [x_1 import-d] [x_2 d_2] ...)
   (append ([x_1 import-d]) (gather-import [x_2 d_2] ...))]
  [(gather-import [x_1 d_1] [x_2 d_2] ...)
   (gather-import [x_2 d_2] ...)])
(define-metafunction SP-compiled
  gather-class : [x d] ... -> ([x (class (e ...) ([x d] ...))] ...)
  [(gather-class)
   ()]
  [(gather-class [x_1 class-d] [x_2 d_2] ...)
   (append ([x_1 class-d]) (gather-class [x_2 d_2] ...))]
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

