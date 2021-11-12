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
      (tuple-syntax (e- ...))
      (set-syntax (e- ...))
      (dict-syntax ([e- e-] ...))
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
        ([x s-+☠] ...))
      ;; new construct! declare (x ...) then do s-
      level-)

  ;; statements
  (s- (expr e-)
      (return e-)
      (assert e-)
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
  (Ψ ([x (class (l ...) Γ ρ)] ...))

  ;; a local environment that maps variables to their types
  (Γ ([x T] ...))

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

  ;; type-op
  (type-op "CheckedDict[_,_]"
           "Optional[_]")

  (exactness exact subof)

  ;; Checkable types are types but without functions
  (checkable-T
   dynamic
   (exactness class-l)
   (Optional nonnonable-and-checkable-T)
   (Type T)
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

  ;; class labels are l's but without constants
  ;;   and the x is limited to builtin classes
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
   "float"
   "int"
   "bool"
   "str"
   "set"
   "list"
   "tuple"
   "dict"
   "type"
   "Optional[_]"
   "CheckedDict[_,_]"
   (tuple (checkable-T ...))
   (chkdict checkable-T checkable-T)))

(define-metafunction SP-compiled
  lookup-Ψ : Ψ class-l -> (class (l ...) Γ ρ)
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
     (["__init__" (-> () dynamic)])
     (["__init__" "object.__init__"]))]
  [(lookup-Ψ Ψ "type")
   (class ("object")
     ()
     ())]
  [(lookup-Ψ Ψ "float")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["__gt__" (-> ((subof "float")) (subof "bool"))]
      ["__lt__" (-> ((subof "float")) (subof "bool"))]
      ["__eq__" (-> ((subof "float")) (subof "bool"))]
      ["__le__" (-> ((subof "float")) (subof "bool"))]
      ["__ge__" (-> ((subof "float")) (subof "bool"))]
      ["__neg__" (-> () (subof "bool"))]
      ["__add__" (-> ((subof "float")) (subof "float"))]
      ["__sub__" (-> ((subof "float")) (subof "float"))]
      ["__mul__" (-> ((subof "float")) (subof "float"))]
      ["__div__" (-> ((subof "float")) (subof "float"))])
     (["__init__" (method "float" "__init__")]
      ["__gt__" (method "float" "__gt__")]
      ["__lt__" (method "float" "__lt__")]
      ["__eq__" (method "float" "__eq__")]
      ["__le__" (method "float" "__le__")]
      ["__ge__" (method "float" "__ge__")]
      ["__neg__" (method "float" "__neg__")]
      ["__add__" (method "float" "__add__")]
      ["__sub__" (method "float" "__sub__")]
      ["__mul__" (method "float" "__mul__")]
      ["__div__" (method "float" "__div__")]))]
  [(lookup-Ψ Ψ "int")
   (class ("float")
     ()
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
     (["__init__" (-> (dynamic) dynamic)])
     (["__init__" (method "str" "__init__")]))]
  [(lookup-Ψ Ψ "dict")
   (class dict "object"
     (["__init__" (-> (dynamic) dynamic)]
      ["__getitem__" (-> (dynamic) dynamic)]
      ["__setitem__" (-> (dynamic dynamic) (subof "NoneType"))]
      ["__delitem__" (-> (dynamic) (subof "NoneType"))])
     (["__init__" (method "dict" "__init__")]
      ["__getitem__" (method "dict" "__getitem__")]
      ["__setitem__" (method "dict" "__setitem__")]
      ["__delitem__" (method "dict" "__delitem__")]))]
  [(lookup-Ψ Ψ "set")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["__contains__" (-> (dynamic) (subof "bool"))])
     (["__init__" (method "set" "__init__")]
      ["__contains__" (method "set" "__contains__")]))]
  [(lookup-Ψ Ψ "list")
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)])
     (["__init__" (method "list" "__init__")]))]
  [(lookup-Ψ Ψ "NoneType")
   (class ("object") () ())]
  [(lookup-Ψ Ψ (chkdict T_key T_val))
   (class ("object")
     (["__init__" (-> (dynamic) dynamic)]
      ["get" (-> (T_key) (union Ψ (subof "NoneType") T_val))]
      ["keys" (-> () (subof "list"))]
      ["__getitem__" (-> ([T_key]) T_val)]
      ["__setitem__" (-> ([T_key] [T_val]) (subof "NoneType"))]
      ["__delitem__" (-> ([T_key]) (subof "NoneType"))])
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
    ["float" (Type (subof "float"))]
    ["int" (Type (subof "int"))]
    ["bool" (Type (subof "bool"))]
    ["str" (Type (subof "str"))]
    ["dict" (Type (subof "dict"))]
    ["set" (Type (subof "set"))]
    ["type" (Type (subof "type"))])])

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
  (test-equal (term (T-of-c 2.3))
              (term (exact "float")))
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
  [(l-of-c number) "float"]
  [(l-of-c string) "str"])

(module+ test
  (check-judgment-holds*
   (Ψ⊢class-l<:class-l () "int" "float")
   (Ψ⊢class-l<:class-l () "bool" "float")
   (Ψ⊢class-l<:class-l () "NoneType" "object")
   (Ψ⊢class-l<:class-l () "NoneType" "NoneType"))
  (check-not-judgment-holds*
   (Ψ⊢class-l<:class-l () "float" "int")
   (Ψ⊢class-l<:class-l () "float" "bool")
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
  (test-equal (term (union (base-Ψ) (subof "float") (subof "int")))
              (term (subof "float")))
  (test-equal (term (union (base-Ψ) (subof "int") (subof "float")))
              (term (subof "float")))
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
  (test-equal (term (union (base-Ψ) (Optional (subof "int")) (Optional (subof "float"))))
              (term (Optional (subof "float")))))
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

(module+ test
  (test-equal (term (lookup-method-T (base-Ψ) "int" "__add__"))
              (term (-> ((subof "float")) (subof "float")))))
(define-metafunction SP-compiled
  lookup-method-T : Ψ l x -> T
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
                            (desugar-e (subscript "CheckedDict" (tuple-syntax ("int" "str"))))))
              (term (subof (chkdict (subof "int") (subof "str"))))))
(define-metafunction SP-compiled
  eval-t : Ψ Γ Γ t -> T
  [(eval-t Ψ Γ_dcl Γ_lcl dynamic)
   dynamic]
  [(eval-t Ψ Γ_dcl Γ_lcl e)
   T
   (where [e- (Type T)] (compile-e Ψ Γ_dcl Γ_lcl e))])

(module+ test
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) "int"))
              (term ["int" (Type (subof "int"))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (con "foo")))
              (term [(ref (con "foo")) (exact "str")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (tuple-syntax ("int" "str"))))
              (term [(tuple-syntax ("int" "str"))
                     (exact (tuple ((Type (subof "int")) (Type (subof "str")))))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (set-syntax ())))
              (term [(set-syntax ()) (exact "set")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (dict-syntax ())))
              (term [(dict-syntax ()) (exact "dict")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (not (con None))))
              (term [(if-exp (ref (con None)) (ref (con #f)) (ref (con #t)))
                     (exact "bool")]))
  (test-match SP-compiled
              [(call-function (lambda (x_tmp) (if-exp x_tmp (ref (con 2)) x_tmp)) ((ref (con 1))))
               dynamic]
              (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (and (con 1) (con 2)))))
  (test-match SP-compiled
              [(call-function (lambda (x_tmp) (if-exp x_tmp x_tmp (ref (con 2)))) ((ref (con 1))))
               dynamic]
              (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (or (con 1) (con 2)))))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (is "int" "bool")))
              (term [(is "int" "bool")
                     (exact "bool")]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (if-exp "int" "bool" (con None))))
              (term [(if-exp "int" "bool" (ref (con None)))
                     (Optional (Type (subof "bool")))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (attribute "int" "__add__")))
              (term [(ref (method "int" "__add__"))
                     (-> ((subof "int") (subof "float")) (subof "float"))]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (call "int" ("float"))))
              (term [(call-function "int" ("float"))
                     dynamic]))
  (test-equal (term (compile-e (base-Ψ) (prim-Γ) (prim-Γ) (lambda () (con 2))))
              (term [(lambda () (ref (con 2)))
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
  [(compile-e Ψ Γ_dcl Γ_lcl (tuple-syntax (e ...)))
   [(tuple-syntax (e- ...))
    (exact (tuple (T ...)))]
   (where ([e- T] ...) ((compile-e Ψ Γ_dcl Γ_lcl e) ...))]
  ;; set literal
  [(compile-e Ψ Γ_dcl Γ_lcl (set-syntax (e ...)))
   [(set-syntax ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) ...))
    (exact "set")]]
  ;; dict literal
  [(compile-e Ψ Γ_dcl Γ_lcl (dict-syntax ([e_key e_val] ...)))
   [(dict-syntax ([(as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_key))
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
       (compile-check checkable-T_dst)
       (return "tmp")))])
(define-metafunction SP-compiled
  compile-e-lambda : Ψ Γ Γ ([x t] ...) e -> [e- T]
  [(compile-e-lambda Ψ Γ_dcl Γ_lcl ([x_arg t_arg] ...) e_out)
   [(lambda (x_arg ...) e-_out)
    (-> (T_arg ...) T_out)]
   (where (T_arg ...) ((eval-t Ψ Γ_dcl Γ_lcl t_arg) ...))
   (where [e-_out T_out] (compile-e Ψ Γ_dcl (extend Γ_dcl [x_arg T_arg] ...) e_out))])
(module+ test
  (test-equal (term (type-call "CheckedDict[_,_]" (exact (tuple ((Type (subof "int"))
                                                                 (Type (subof "str")))))))
              (term (Type (subof (chkdict (subof "int") (subof "str"))))))
  (test-equal (term (type-call "Optional[_]" (Type (exact "int"))))
              (term (Type (Optional (exact "int"))))))
(define-metafunction SP-compiled
  type-call : type-op T -> T
  [(type-call "CheckedDict[_,_]" (exact (tuple ((Type T_key) (Type T_val)))))
   (Type (subof (chkdict T_key T_val)))]
  [(type-call "Optional[_]" (Type nonnonable-T))
   (Type (Optional nonnonable-T))])
(module+ test
  (test-equal (term (compile-e-call (base-Ψ) (prim-Γ) (prim-Γ)
                                    (attribute (con 2) "__add__") ((con 3))))
              (term [(invoke-function (method "int" "__add__") ((ref (con 2)) (ref (con 3))))
                     (subof "float")]))
  (test-equal (term (compile-e-call (base-Ψ) (prim-Γ) (prim-Γ)
                                    (attribute "CheckedDict" "__getitem__")
                                    ((tuple-syntax ("int" "str")))))
              (term [(invoke-function (method "CheckedDict[_,_]" "__getitem__")
                                      ("CheckedDict"
                                       (tuple-syntax ("int" "str"))))
                     (Type (subof (chkdict (subof "int") (subof "str"))))])))
(define-metafunction SP-compiled
  compile-e-call : Ψ Γ Γ e (e ...) -> [e- T]
  ;; type-level calls
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   [(invoke-function l_mth (e-_obj e-_arg ...))
    (type-call type-op T_arg ...)]
   (where [e-_obj (Type (subof l_cls))] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where type-op (lookup-method-T Ψ l_cls x_mth))
   (where l_mth (lookup-method-l Ψ l_cls x_mth))
   (where ([e-_arg T_arg] ...) ((compile-e Ψ Γ_dcl Γ_lcl e_arg) ...))]
  ;; we emit INVOKE_FUNCTION when the exact class is known
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   [(invoke-function l_mth (e-_obj (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where [e-_obj (exact l_cls)] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where (-> (T_arg ...) T_out) (lookup-method-T Ψ l_cls x_mth))
   (where l_mth (lookup-method-l Ψ l_cls x_mth))]
  ;; we emit INVOKE_METHOD when the class is known but inexact
  [(compile-e-call Ψ Γ_dcl Γ_lcl (attribute e_obj x_mth) (e_arg ...))
   [(invoke-method l_cls x_mth e-_obj ((maybe-cast (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...))
    T_out]
   (where [e-_obj (subof l_cls)] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where (-> (T_arg ...) T_out) (lookup-method-T Ψ l_cls x_mth))]
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
   (where [e- (exactness l)] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where T (lookup-method-T Ψ l x))]
  [(compile-e-attribute Ψ Γ_dcl Γ_lcl e x)
   [(ref l_mth)
    (-> ((subof l_cls) T_arg ...) T_out)]
   (where [e- (Type (subof l_cls))] (compile-e Ψ Γ_dcl Γ_lcl e))
   (where (-> (T_arg ...) T_out) (lookup-method-T Ψ l_cls x))
   (where l_mth (lookup-method-l Ψ l_cls x))]
  [(compile-e-attribute Ψ Γ_dcl Γ_lcl e x)
   [(safe-attribute (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)) x)
    dynamic]])
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
   (compile-e-if-exp Ψ Γ_dcl Γ_lcl e_1 (if e_2 e_thn e_els) e_els)]
  ;; step case -- or
  [(compile-e-if-exp Ψ Γ_dcl Γ_lcl (or e_1 e_2) e_thn e_els)
   (compile-e-if-exp Ψ Γ_dcl Γ_lcl e_1 e_thn (if e_2 e_thn e_els))]
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
  compile-s : Ψ Γ Γ T+☠ s -> s-
  ;; expr
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (expr e))
   (expr (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)))]
  ;; return
  [(compile-s Ψ Γ_dcl Γ_lcl T (return e))
   (return (maybe-cast Ψ (compile-e Ψ Γ_dcl Γ_lcl e) T))]
  ;; assert
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (assert e))
   (assert (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e)))]
  ;; begin
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (begin s ...))
   (compile-begin Ψ Γ_dcl Γ_lcl T+☠ s ...)]
  ;; if TODO need to revise this. if should be more sophisticated
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (if e_cnd s_thn s_els))
   (if (as-dyn (compile-e Ψ Γ_dcl Γ_lcl e_cnd))
       (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_thn)
       (compile-s Ψ Γ_dcl Γ_lcl T+☠ s_els))]
  ;; TODO delete x
  ;; TODO delete attr
  ;; ann are ignored
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (ann x t))
   (begin)]
  ;; ann-assign x
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (ann-assign x t e))
   (assign x e-)
   (where [e- T_lcl] (compile-e Ψ Γ_dcl Γ_lcl e))]
  ;; ann-assign attribute
  ;; TODO
  ;; function-def
  ;; TODO
  [(compile-s Ψ Γ_1 Γ_lcl T+☠ (function-def x ([x_arg t_arg] ...) t_ret d s))
   (function-def x ([x_arg (lookup Γ_3 x_arg)] ...)
     (compile-d d)
     (compile-s Ψ Γ_3 Γ_3 T_ret s))
   (judgment-holds (eval-t Ψ Γ_1 t_ret T_ret))
   (where (([x_cls any] ...) ([x_var D_var] ...)) (split-d d))
   (where Γ_2 (extend Γ_1 [x_cls dynamic] ... [x_var ☠] ...))
   (judgment-holds (evalD* Ψ Γ_2 (D_var ...) (T_var ...)))
   (where Γ_3 (update Γ_2 [x_var T_var] ...))]
  ;; class
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (class x (t ...) m ...))
   (class x ((as-dyn (compile-e Ψ Γ_dcl Γ_lcl t)) ...)
     (compile-m Ψ Γ_dcl x m) ...)]
  ;; import-from
  [(compile-s Ψ Γ_dcl Γ_lcl T+☠ (import-from x_mod x_var))
   (import-from x_mod x_var)])
(define-metafunction SP-compiled
  compile-begin : s ... -> s-
  [(compile-begin Ψ Γ_dcl Γ_lcl ☠)
   (begin)]
  [(compile-begin Ψ Γ_dcl Γ_lcl T)
   (compile-s Ψ Γ_dcl Γ_lcl T+☠ (return (con None)))]
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
#|
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


(define-judgment-form SP-compiled
  #:mode (Ψ⊢T<:T I I I)
  #:contract (Ψ⊢T<:T Ψ T T)
  [(where #t (= (union Ψ T_src T_tgt) T_tgt))
   --------------------
   (Ψ⊢T<:T Ψ T_src T_tgt)])

(define-metafunction SP-compiled
  maybe-cast! : Ψ (e- T) T -> e-
  [(maybe-cast! Ψ (e-_ins T_src) T_tgt)
   e-_ins
   (judgment-holds (Ψ⊢T<:T Ψ T_src T_tgt))]
  [(maybe-cast! Ψ (e-_ins T_src) checkable-T_tgt)
   (let ([tmp e-_ins])
     (begin
       (compile-check checkable-T_tgt tmp)
       (return tmp)))])

(define-metafunction SP-compiled
  compile-check : checkable-T_tgt e- -> s-
  [(compile-check dynamic e-) (begin)]
  [(compile-check (instancesof cid) e-)
   (assert ((ref "isinstance") e- (ref cid)))]
  [(compile-check (Optional cid) e-)
   (if (is e- None)
       (begin)
       (compile-check (instancesof cid) e-))])

(module+ test
  (test-equal (term (compile-s (base-Ψ)
                               (extend (base-Γ) [x dynamic])
                               (extend (base-Γ) [x dynamic])
                               ☠
                               (begin
                                 (ann-assign x dynamic 2)
                                 (expr ((attribute x "__add__") 3)))))
              (term (begin
                      (ann-assign x (ref (con 2)))
                      (expr ((ref (attribute "float" "__add__")) x (ref (con 3))))))))

(define-metafunction SP-compiled
  compile-m : Ψ Γ t m -> m-
  [(compile-m Ψ Γ t_cls (field string t_fld))
   (field string (as-dyn (compile-e Ψ Γ Γ t_fld)))]
  [(compile-m Ψ Γ t_slf (method string_method x_slf ([x_arg t_arg] ...) t_ret d s))
   (method string_method x_slf ([x_arg e-_arg] ...) e-_ret d- s-)
   (where (def tmp ([x_slf e-_slf] [x_arg e-_arg] ...)
            e-_ret
            d-
            s-)
          (compile-s
           Ψ Γ Γ ☠
           (def tmp ([x_slf t_slf] [x_arg t_arg] ...)
             t_ret
             d
             s)))])

(module+ test
  (test-equal (term (compile-e (base-Ψ) (base-Γ) (base-Γ) ((attribute 2 "__add__") 3)))
              (term [((ref (attribute "float" "__add__"))
                      (ref (con 2))
                      (ref (con 3)))
                     (instancesof "float")]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ) [x dynamic])
                               (extend (base-Γ) [x (instancesof "int")])
                               ((attribute x "__add__") 3)))
              (term [((ref (attribute "float" "__add__"))
                      x
                      (ref (con 3)))
                     (instancesof "float")]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               ((attribute CheckedDict "__getitem__")
                                (tuple-syntax int str))))
              (term [((dynamic-attribute CheckedDict "__getitem__")
                      (tuple-syntax int str))
                     (classitself ("CheckedDict" (instancesof "int")
                                                 (instancesof "str")))]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")]
                                       [Optional (prim-generic Optional)])
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")]
                                       [Optional (prim-generic Optional)])
                               ((attribute CheckedDict "__getitem__")
                                (tuple-syntax str
                                              ((attribute Optional "__getitem__") str)))))
              (term [((dynamic-attribute CheckedDict "__getitem__")
                      (tuple-syntax str ((dynamic-attribute Optional "__getitem__") str)))
                     (classitself ("CheckedDict" (instancesof "str")
                                                 (Optional "str")))]))
  (test-equal (term (compile-e (base-Ψ)
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               (extend (base-Γ)
                                       [CheckedDict (prim-generic "CheckedDict")])
                               (((attribute CheckedDict "__getitem__")
                                 (tuple-syntax int str))
                                (dict-syntax))))
              (term [(((dynamic-attribute CheckedDict "__getitem__")
                       (tuple-syntax int str))
                      (dict-syntax))
                     (instancesof ("CheckedDict" (instancesof "int")
                                                 (instancesof "str")))]))
  )
(define-metafunction SP-compiled
  compile-e : Ψ Γ Γ e -> (e- T)
  ;; variable
  [(compile-e Ψ Γ_dcl Γ_lcl x)
   [x T]
   (where T (lookup Γ_lcl x))]
  ;; constant
  [(compile-e Ψ Γ_dcl Γ_lcl c)
   [(ref (con c))
    (instancesof (type-of-c c))]]
  ;; set literal
  [(compile-e Ψ Γ_dcl Γ_lcl (set-syntax e ...))
   [(set-syntax (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
    (instancesof "set")]]
  ;; tuple literal
  [(compile-e Ψ Γ_dcl Γ_lcl (tuple-syntax e ...))
   [(tuple-syntax (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) ...)
    (instancesof "tuple")]]
  ;; dict literal
  [(compile-e Ψ Γ_dcl Γ_lcl (dict-syntax [e_key e_val] ...))
   [(dict-syntax [(get-e (compile-e Ψ Γ_dcl Γ_lcl e_key)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_val))] ...)
    (instancesof "dict")]]
  ;; is
  [(compile-e Ψ Γ_dcl Γ_lcl (is e_lft e_rht))
   [(is (get-e (compile-e Ψ Γ_dcl Γ_lcl e_lft)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_rht)))
    (instancesof "bool")]]
  ;; is-not
  [(compile-e Ψ Γ_dcl Γ_lcl (is-not e_lft e_rht))
   [(is-not (get-e (compile-e Ψ Γ_dcl Γ_lcl e_lft)) (get-e (compile-e Ψ Γ_dcl Γ_lcl e_rht)))
    (instancesof "bool")]]
  ;; if  TODO need refinement
  [(compile-e Ψ Γ_dcl Γ_lcl (if e_cnd e_thn e_els))
   ((if (get-e (compile-e Ψ Γ_dcl Γ_lcl e_cnd))
        e-_thn
        e-_els)
    (union Ψ T_thn T_els))
   (where [e-_thn T_thn] (compile-e Ψ Γ_dcl Γ_lcl e_thn))
   (where [e-_els T_els] (compile-e Ψ Γ_dcl Γ_lcl e_els))]
  ;; generic class
  [(compile-e Ψ Γ_dcl Γ_lcl ((attribute x "__getitem__") (tuple-syntax t ...)))
   [(ref cid) (classitself cid)]
   (judgment-holds (evalo Ψ Γ_lcl ((attribute x "__getitem__") (tuple-syntax t ...)) (instancesof cid)))]
  ;; attribute
  [(compile-e Ψ Γ_dcl Γ_lcl (attribute e string))
   [(dynamic-attribute (get-e (compile-e Ψ Γ_dcl Γ_lcl e)) string)
    dynamic]]
  ;; Optimization!!
  ;; TODO debug
  [(compile-e Ψ Γ_dcl Γ_lcl ((attribute e_obj string_mth) e_arg ...))
   [((ref l_mth) e-_obj (maybe-cast! Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...)
    T_ret]
   (where [e-_obj T_obj] (compile-e Ψ Γ_dcl Γ_lcl e_obj))
   (where (instancesof cid) T_obj)
   (where (-> ([x+☠_prm T_arg] ...) T_ret) (lookup-member Ψ T_obj string_mth))
   (where l_mth (lookup-member-label Ψ T_obj string_mth))
   (where #t (= (len (T_arg ...)) (len (e_arg ...))))]
  [(compile-e Ψ Γ_dcl Γ_lcl (e_fun e_arg ...))
   [(e-_fun (maybe-cast! Ψ (compile-e Ψ Γ_dcl Γ_lcl e_arg) T_arg) ...)
    T_ret]
   (where [e-_fun T_fun] (compile-e Ψ Γ_dcl Γ_lcl e_fun))
   (judgment-holds (as-fun Ψ T_fun (len (e_arg ...)) (-> ([x+☠_arg T_arg] ...) T_ret)))])


(define-metafunction SP-compiled
  lookup-member-label : Ψ T string -> l
  ;; TODO: this looks a bit too ad hoc ...
  [(lookup-member-label Ψ (instancesof cid) string)
   (attribute cid string)
   (where C (lookup-class Ψ cid))
   (where (class any_nam
            ;; parents
            cid+dynamic+☠
            ;; fields
            ([string_fld T_fld] ...)
            ;; methods
            ([string_mth ([x+☠_arg T_arg] ...) T_ret] ...))
          C)
   (where (any_1 ... string any_2 ...)
          (string_fld ... string_mth ...))]
  [(lookup-member-label Ψ (instancesof cid_slf) string)
   (lookup-member-label Ψ (instancesof cid_prn) string)
   (where C (lookup-class Ψ cid_slf))
   (where (class any_nam
            ;; parents
            cid_prn
            ;; fields
            ([string_fld T_fld] ...)
            ;; methods
            ([string_mth ([x+☠_arg T_arg] ...) T_ret] ...))
          C)])


(define-metafunction SP-compiled
  l-of-builtin-class : T -> l
  [(l-of-builtin-class (instancesof string))
   string]
  [(l-of-builtin-class (instancesof ("CheckedDict" T_key T_val)))
   (checked-dict (l-of-builtin-class T_key)
                 (l-of-builtin-class T_val))])

(define-judgment-form SP-compiled
  #:mode (is-builtin-class I)
  #:contract (is-builtin-class T)

  ;; basically, primitive-cid recursively.

  [----------------------------------
   (is-builtin-class (instancesof string))]

  [(is-builtin-class T_key)
   (is-builtin-class T_val)
   ----------------------------------
   (is-builtin-class (instancesof ("CheckedDict" T_key T_val)))])

(define-metafunction SP-compiled
  compile-e* : Ψ Γ Γ e ... -> ([e- T] ...)
  [(compile-e* Ψ Γ_dcl Γ_lcl) ()]
  [(compile-e* Ψ Γ_dcl Γ_lcl e_1 e_2 ...)
   ([e-_1 T_1] [e-_2 T_2] ...)
   (where [e-_1 T_1]
          (compile-e Ψ Γ_dcl Γ_lcl e_1))
   (where ([e-_2 T_2] ...)
          (compile-e* Ψ Γ_dcl Γ_lcl e_2 ...))])
|#