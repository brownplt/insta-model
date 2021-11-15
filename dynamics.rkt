#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(require "grammar.rkt")
(require "desugar.rkt")
(require "compile.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-compiled
  ;; We extended global names to create heap labels
  (l .... number)
  ;; heaps map global names to heap allocated things
  (Σ ([number h] ...))
  ;; heap allocated things includes objects, and environments
  ;; the ☠ represents uninitialized variables
  (h object
     (env ([x v+☠] ...) l+☠))
  ;; According to Python's Data model: "every object has an identity,
  ;; a type and a value"
  ;; We don't need to store the identity here -- the heap Σ already did it.
  ;; But we still need a type (v), and an object value.
  ;; We actually need two things to represent an object values:
  ;;   - primitive values
  ;;   - key-attribute map
  (object (obj v g ([x v] ...)))
  ;; Object values in the sense of Python's Data model.
  (g (con c)
     (tuple (v ...))
     (set (v ...))
     (dict ([v v] ...))
     (lambda v (x ...) s- level-)
     (class (v ...) ([string v] ...))
     (prim-op builtin)
     (method v v)
     (nothing))
  ;; runtime involves applied functions
  (e- .... (local l s-))
  ;; runtime representation of programs
  (p [Σ l (s- ...)]
     (error))
  (P [Σ l (v ... S s- ...)])
  ;; expression contexts
  (E hole
     (tuple (v ... E e- ...))
     (set (v ... E e- ...))
     (dict ([v v] ... [E e-] [e- e-] ...))
     (dict ([v v] ... [v E] [e- e-] ...))
     (is E e-)
     (is v E)
     (if E e- e-)
     (attribute mode E x)
     (invoke-function l (v ... E e- ...))
     (invoke-method l x E (e- ...))
     (invoke-method l x v (v ... E e- ...))
     (call-function E (e- ...))
     (call-function v (v ... E e- ...))
     (class (v ... E e- ...) ([x s-+☠] ...)))
  ;; statement contexts
  (S (expr E)
     (return E)
     (if E s- s-)
     (delete (attribute E x))
     (assign x E)
     (assign (attribute E x) e-)
     (assign (attribute v x) E))
  (builtin-op
   "isinstance"
   (attribute "float" "__add__")
   (attribute "float" "__sub__")
   (attribute "float" "__mul__")
   (attribute "float" "__div__")
   (attribute "dict" "__init__")
   (attribute "dict" "__getitem__")
   (attribute "dict" "__setitem__")
   (attribute "dict" "__delitem__")
   (attribute "CheckedDict" "__getitem__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__init__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__getitem__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__setitem__")
   (attribute ("CheckedDict" checkable-T checkable-T) "__delitem__"))
  ;; utilities
  (v+☠ v ☠)
  (l+☠ l ☠)
  )

(module+ test
  (test-equal (term (alloc ()))
              (term (())))
  (test-equal (term (alloc ()
                           (obj (ref "int") (con 2) ())
                           (env () "builtin-env")))
              (term (([1 (env () "builtin-env")]
                      [0 (obj (ref "int") (con 2) ())])
                     0
                     1))))
(define-metafunction SP-dynamics
  alloc : Σ h ... -> (Σ l ...)
  [(alloc Σ)
   (Σ)]
  [(alloc Σ h)
   ((extend Σ [l h]) l)
   (where l ,(length (term Σ)))]
  [(alloc Σ_1 h_1 h_2 ...)
   (Σ_3 l_1 l_2 ...)
   (where (Σ_2 l_1) (alloc Σ_1 h_1))
   (where (Σ_3 l_2 ...) (alloc Σ_2 h_2 ...))])

(module+ test
  (test-match SP-dynamics Σ (term (base-Σ))))
(define-metafunction SP-dynamics
  base-Σ : -> Σ
  [(base-Σ)
   ()])
(module+ test
  (test-equal (term
               (lookup-Σ (base-Σ) "builtin-env"))
              (term
               (env
                (["object" (ref "object")]
                 ["int" (ref "int")]
                 ["bool" (ref "bool")]
                 ["str" (ref "str")]
                 ["dict" (ref "dict")]
                 ["set" (ref "set")]
                 ["type" (ref "type")]
                 ["isinstance" (ref "isinstance")]
                 ["len" (ref "len")]
                 ["Exception" (ref "Exception")]
                 ["max" (ref "max")]
                 ["min" (ref "min")]
                 ["issubclass" (ref "issubclass")])
                ☠))))
(define-metafunction SP-dynamics
  lookup-Σ : Σ l -> h
  [(lookup-Σ Σ number)
   (lookup Σ number)]
  [(lookup-Σ Σ l)
   (lookup-Σ-primitive l)])
(define-metafunction SP-dynamics
  lookup-Σ-primitive : l -> h
  [(lookup-Σ-primitive "builtin-env")
   (env ([x (ref x)] ...) ☠)
   (where ([x T] ...) (base-Γ))])


(module+ test
  (test-equal (term (update-env ([0 (env (["x" (ref "int")]) ☠)]) 0
                                "x" (ref "bool")))
              (term ([0 (env (["x" (ref "bool")]) ☠)])))
  (test-equal (term (update-env ([1 (env () 0)]
                                 [0 (env (["x" (ref "int")]) ☠)]) 1
                                "x" (ref "bool")))
              (term ([1 (env () 0)]
                     [0 (env (["x" (ref "bool")]) ☠)]))))
(define-metafunction SP-dynamics
  update-env : Σ l x v -> Σ
  [(update-env Σ l x v_new)
   (update Σ [l (env (any_1 ... [x v_new] any_2 ...) l+☠_out)])
   (where (env (any_1 ... [x v_old] any_2 ...) l+☠_out) (lookup-Σ Σ l))]
  [(update-env Σ l x v)
   (update-env Σ l_out x v)
   (where (env any_map l_out) (lookup-Σ Σ l))])


(module+ test
  (test-equal (term (super-load ()))
              (term [([1 (env () 0)]
                      [0
                       (env
                        (["object" (ref "object")]
                         ["int" (ref "int")]
                         ["bool" (ref "bool")]
                         ["str" (ref "str")]
                         ["dict" (ref "dict")]
                         ["set" (ref "set")]
                         ["type" (ref "type")]
                         ["isinstance" (ref "isinstance")]
                         ["len" (ref "len")]
                         ["Exception" (ref "Exception")]
                         ["max" (ref "max")]
                         ["min" (ref "min")]
                         ["issubclass" (ref "issubclass")])
                        ☠)])
                     1
                     ((begin))])))
(define-metafunction SP-dynamics
  super-load : program+ -> [Σ l (s- ...)]
  [(super-load program+)
   (load (compile-program (desugar-program program+)))])
(define-metafunction SP-dynamics
  load : program- -> [Σ l (s- ...)]
  [(load (local (x ...) s-))
   [Σ_2 l_2 (s-)]
   (where ([x_builtin T] ...) (base-Γ))
   (where (Σ_1 l_1) (alloc (base-Σ) (env ([x_builtin (ref x_builtin)] ...) ☠)))
   (where (Σ_2 l_2) (alloc Σ_1 (env ([x ☠] ...) l_1)))])

#|
(define red-e
  (reduction-relation
    SP-dynamics
    #:domain [Σ l e-]
    [--> [Σ l x]
         [Σ l v]
         (where v-env (lookup (lookup Σ l) x))]))

(module+ test
  (test--> red-p
     (term (super-load ((ann-assign "i" "int" (con 42)))))
     (term [()
            0
            ()]))
  (test--> red-p
     (term (super-load ((ann-assign "i" "int" (con 42))
                        (expr "i"))))
     (term [()
            0
            ((ref (con 42)))]))
)

(define red-p
  (reduction-relation
   SP-dynamics
   #:domain p
   [--> (Σ l (v_1 ... (expr v_n) s- ...))
        (Σ l (v_1 ... v_n s- ...))
        "expr-terminate"]
   [--> (Σ l (v_1 ... (begin s-_1 ...) s-_2 ...))
        (Σ l (v_1 ... s-_1 ... s-_2 ...))
        "begin"]
   [--> (Σ_1 l (v ... (assign x_var v_new) s- ...))
        (Σ_2 l (v ... s- ...))
        (where Σ_2 (update-env Σ_1 l x_var v_new))
        "assign"]
   [--> (Σ_1 l (v_1 ... (expr e-_1) s- ...))
        (Σ_2 l (v_1 ... (expr e-_2) s- ...))
        (where [Σ_2 l e-_2]
               ,(apply-reduction-relation red-e (term [Σ_1 l e-_1])))
        "expr"]))

; (define red-s
;   (reduction-relation
;   SP-dynamics
;   #:domain (s)
;   [(expr )]))
#|
(define e→e
  (reduction-relation
   SP-dynamics
   #:domain (Σ ρ e-)
   [--> (in-hole (Σ ρ E) x)
        (in-hole (Σ ρ E) (lookup Σ l))
        (where l (lookup ρ x))
        "lookup"]
   [--> (in-hole (Σ ρ E) (con c))
        (in-hole (Σ ρ E) (ref (con c)))
        "constant"]
   [--> (in-hole (Σ_1 ρ E) (tuple v ...))
        (in-hole (Σ_2 ρ E) (ref l))
        (where (Σ_2 l) (alloc Σ_1 ("tuple" (tuple v ...))))
        "tuple"]
   [--> (in-hole (Σ_1 ρ E) (set v ...))
        (in-hole (Σ_2 ρ E) (ref l))
        (where (Σ_2 l) (alloc Σ_1 ("set" (set v ...))))
        "set"]
   [--> (in-hole (Σ_1 ρ E) (dict [v_key v_val] ...))
        (in-hole (Σ_2 ρ E) (ref l))
        (where (Σ_2 l) (alloc Σ_1 ("dict" (dict [v_key v_val] ...))))
        "dict"]
   [--> (in-hole (Σ ρ E) (is v_1 v_2))
        (in-hole (Σ ρ E) (ref (con (= v_1 v_2))))
        "is"]
   [--> (in-hole (Σ ρ E) (is-not v_1 v_2))
        (in-hole (Σ ρ E) (ref (con (≠ v_1 v_2))))
        "is-not"]
   [--> (in-hole (Σ ρ E) (if (ref l_cnd) e-_thn e-_els))
        (in-hole (Σ ρ E) (do-if (lookup-Σ Σ l_cnd) e-_thn e-_els))
        "if"]
   [--> (in-hole (Σ_1 ρ E) (dynamic-attribute v_map string))
        (in-hole (Σ_2 ρ E) r_val)
        (where (Σ_2 r_val) (get-attr Σ_1 v_map string))
        "dynamic-attribute"]
   [--> (in-hole (Σ_1 ρ E) (let ([x v] ...) s-))
        (in-hole (Σ_2 ρ E) (enter (extend ρ [x l] ...) s-))
        (where (Σ_2 l ...) (alloc Σ_1 v ...))
        "let"]
   [--> (in-hole (Σ ρ_1 E) (enter ρ_2 s-))
        (in-hole (Σ ρ_2 E) (leave ρ_1 s-))
        "enter"]
   [--> (in-hole (Σ ρ_1 E) (leave ρ_2 (return v)))
        (in-hole (Σ ρ_2 E) v)
        "leave"]
   [--> (in-hole (Σ_1 ρ E) (v_fun v_arg ...))
        (in-hole (Σ_2 ρ E) e-_ret)
        (where (Σ_2 e-_ret) (do-app Σ_1 v_fun v_arg ...))
        "procedure call / function application / function call"]))

(module+ test
  (test-equal (term (lookup-Σ (base-Σ) (con 2)))
              (term ("int" 2)))
  (test-equal (term (lookup-Σ (base-Σ) (con #t)))
              (term ("bool" #t)))
  (test-equal (term (lookup-Σ (base-Σ) (con #f)))
              (term ("bool" #f))))
(define-metafunction SP-dynamics
  lookup-Σ : Σ l -> h
  [(lookup-Σ Σ number) (lookup Σ number)]
  [(lookup-Σ Σ builtin) (lookup-builtin builtin)])
(define-metafunction SP-dynamics
  lookup-builtin : builtin -> h
  [(lookup-builtin (con integer))
   ("int" integer)]
  [(lookup-builtin (con number))
   ("float" number)]
  [(lookup-builtin (con boolean))
   ("bool" boolean)]
  [(lookup-builtin (con string))
   ("str" string)]
  [(lookup-builtin (con None))
   ("NoneType" None)]
  [(lookup-builtin "function")
   ("type" (class ("object") ()))]
  [(lookup-builtin "float")
   ("type" (class ("object")
             (["__gt__" (attribute "float" "__gt__")]
              ["__eq__" (attribute "float" "__eq__")]
              ["__neq__" (attribute "float" "__neq__")]
              ["__add__" (attribute "float" "__add__")]
              ["__sub__" (attribute "float" "__sub__")])))]
  [(lookup-builtin "int")
   ("type" (class ("float") ()))]
  [(lookup-builtin "bool")
   ("type" (class ("int") ()))]
  [(lookup-builtin "str")
   ("type" (class ("object") ()))]
  [(lookup-builtin "tuple")
   ("type" (class ("object") ()))]
  [(lookup-builtin "set")
   ("type" (class ("object") ()))]
  [(lookup-builtin "dict")
   ("type" (class ("object")
             (["__setitem__" (attribute "dict" "__setitem__")]
              ["__getitem__" (attribute "dict" "__getitem__")]
              ["__delitem__" (attribute "dict" "__delitem__")])))]
  [(lookup-builtin ("CheckedDict" checkable-T_key checkable-T_val))
   ("type" (class ("object")
             (["__init__" (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__init__")]
              ["__setitem__" (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__setitem__")]
              ["__getitem__" (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__getitem__")]
              ["__delitem__" (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__delitem__")])))]
  [(lookup-builtin "CheckedDict")
   ("type" (class ("object")
             (["__getitem__" (attribute "CheckedDict" "__getitem__")])))]
  [(lookup-builtin "object")
   ("type" (class () (["__init__" (attribute "object" "__init__")])))]
  [(lookup-builtin "type")
   ("type" (class ("object") ()))]
  [(lookup-builtin builtin-op)
   ("primitive_operator" (prim-op builtin-op))])
(module+ test
  (test-equal (term (delta (base-Σ) "isinstance" (ref (con 23)) (ref "int")))
              (term ((base-Σ) (ref (con #t))))))
(define-metafunction SP-dynamics
  delta : Σ builtin v ... -> (Σ e-)

  [(delta Σ (attribute "object" "__init__") v_slf v_arg ...)
   (Σ v_slf)]

  [(delta Σ (attribute "float" "__add__") (ref (con number_1)) (ref (con number_2)))
   (Σ (ref (con ,(+ (term number_1) (term number_2)))))]
  [(delta Σ (attribute "float" "__sub__") (ref (con number_1)) (ref (con number_2)))
   (Σ (ref (con ,(- (term number_1) (term number_2)))))]
  [(delta Σ (attribute "float" "__mul__") (ref (con number_1)) (ref (con number_2)))
   (Σ (ref (con ,(* (term number_1) (term number_2)))))]
  [(delta Σ (attribute "float" "__div__") (ref (con number_1)) (ref (con number_2)))
   (Σ (ref (con ,(/ (term number_1) (term number_2)))))]

  ;; dict operators
  [(delta Σ (attribute "dict" "__getitem__") (ref l_map) v_key)
   (Σ v_val)
   (where (l_cls (dict any_1 ... [v_key v_val] any_2 ...))
          (lookup-Σ Σ l_map))]
  [(delta Σ_1 (attribute "dict" "__setitem__") (ref l_map) v_key v_new)
   (Σ_2 (ref (con None)))
   (where (l_cls (dict any_1 ... [v_key v_old] any_2 ...))
          (lookup-Σ Σ_1 l_map))
   (where Σ_2 (update Σ_1 [l_map (l_cls (dict any_1 ... [v_key v_new] any_2 ...))]))]
  [(delta Σ_1 (attribute "dict" "__setitem__") (ref l_map) v_key v_val)
   (Σ_2 (ref (con None)))
   (where (l_cls (dict any ...))
          (lookup-Σ Σ_1 l_map))
   (where Σ_2 (update Σ_1 [l_map ("dict" (dict [v_key v_val] any ...))]))]
  [(delta Σ_1 (attribute "dict" "__delitem__") (ref l_map) v_key)
   (Σ_2 (ref (con None)))
   (where (l_cls (dict any_1 ... [v_key v_val] any_2 ...))
          (lookup-Σ Σ_1 l_map))
   (where Σ_2 (update Σ_1 [l_map (l_cls (dict any_1 ... any_2 ...))]))]

  [(delta Σ "isinstance" (ref l_ins) (ref l_tgt))
   (Σ (ref (con (issubclass Σ l_src l_tgt))))
   (where (l_src _) (lookup-Σ Σ l_ins))]

  [(delta Σ (attribute "CheckedDict" "__getitem__") (ref l_tpl))
   (Σ (ref ("CheckedDict" (instancesof l_key) (instancesof l_val))))
   (where ("tuple" (tuple (ref l_key) (ref l_val))) (lookup-Σ Σ l_tpl))
   (where ("type" any_key) (lookup-Σ Σ l_key))
   (where ("type" any_val) (lookup-Σ Σ l_val))]

  [(delta Σ_1 (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__init__")
          (ref l_obj) (ref l_dct))
   (Σ_2 (let ()
          (begin
            (compile-check checkable-T_key v_key) ...
            (compile-check checkable-T_val v_val) ...
            (return None))))
   (where ("dict" (dict [v_key v_val] ...))
          (lookup-Σ Σ_1 l_dct))
   (where Σ_2 (update Σ_1 [l_obj (("CheckedDict" checkable-T_key checkable-T_val)
                                  (dict [v_key v_val] ...))]))]

  [(delta Σ (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__getitem__") v_obj v_key)
   (Σ (let ()
        (begin
          (compile-check checkable-T_key v_key)
          (return ((ref (attribute "dict" "__getitem__")) v_obj v_key)))))]

  [(delta Σ (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__setitem__") v_obj v_key v_val)
   (Σ (let ()
        (begin
          (compile-check checkable-T_key v_key)
          (compile-check checkable-T_val v_val)
          (return ((ref (attribute "dict" "__setitem__")) v_obj v_key v_val)))))]

  [(delta Σ (attribute ("CheckedDict" checkable-T_key checkable-T_val) "__delitem__") v_obj v_key)
   (Σ (let ()
        (begin
          (compile-check checkable-T_key v_key)
          (return ((ref (attribute "dict" "__delitem__")) v_obj v_key)))))]

  [(delta Σ builtin v ...)
   (Σ ☠)
   #;(judgment-holds (show ((do-delta builtin v ...) is-undefined)))
   #;(judgment-holds (show (the-context-is Σ)))])

(define-metafunction SP-dynamics
  let* : ([x e-] ...) s- -> e-
  [(let* () e-) e-]
  [(let* ([x_1 e-_1] [x_2 e-_2] ...) s-)
   (let ([x_1 e-_1])
     (let* ([x_2 e-_2] ...) s-))])

(define-metafunction SP-dynamics
  issubclass : Σ l l -> boolean
  [(issubclass Σ l l) #t]
  [(issubclass Σ "object" l_tgt) #f]
  [(issubclass Σ l_src l_tgt)
   (issubclass Σ l_par l_tgt)
   (where ("type" (class (l_par) _)) (lookup-Σ Σ l_src))])

(define builtin-names
  (list "object"
        "float"
        "int"
        "bool"
        "str"
        "dict"
        "set"
        "type"))

(define-metafunction SP-dynamics
  base-Σ : -> Σ
  [(base-Σ)
   ,(for/list ([name builtin-names]
               [i (in-naturals)])
      `[,i (ref ,name)])])

(define-metafunction SP-dynamics
  base-ρ : -> ρ
  [(base-ρ)
   ,(for/list ([name builtin-names]
               [i (in-naturals)])
      `[,(string->symbol name) ,i])])

(module+ test
  (test-equal (term #t) (term (falsy ("NoneType" None))))
  (test-equal (term #t) (term (falsy ("float" 0.0))))
  (test-equal (term #t) (term (falsy ("int" 0))))
  (test-equal (term #t) (term (falsy ("bool" #f))))
  (test-equal (term #t) (term (falsy ("tuple" (tuple)))))
  (test-equal (term #t) (term (falsy ("set" (set)))))
  (test-equal (term #t) (term (falsy ("dict" (dict)))))
  (test-equal (term #f) (term (falsy ("int" 2))))
  (test-equal (term #f) (term (falsy ("bool" #t))))
  (test-equal (term #f) (term (falsy ("tuple" (tuple (ref (con None))))))))
(define-metafunction SP-dynamics
  falsy : h -> boolean
  [(falsy ("NoneType" None)) #t]
  [(falsy ("float" 0.0)) #t]
  [(falsy ("int" 0)) #t]
  [(falsy ("bool" #f)) #t]
  [(falsy ("tuple" (tuple))) #t]
  [(falsy ("set" (set))) #t]
  [(falsy ("dict" (dict))) #t]
  [(falsy h) #f])

(module+ test
  (test-equal (term (get-attr (base-Σ) (ref "float") "__add__"))
              (term ((base-Σ) (ref (attribute "float" "__add__")))))
  (test-equal (term (get-attr (base-Σ) (ref "bool") "__add__"))
              (term ((base-Σ) (ref (attribute "float" "__add__"))))))
(define-metafunction SP-dynamics
  get-attr : Σ v string -> (Σ r)
  ;; TODO: it is unclear to me how this function should work
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
          (lookup-Σ Σ l_slf))]
  [(get-attr Σ (ref l_slf) string_key)
   (Σ ☠)
   (where ("type" any) (lookup-Σ Σ l_slf))]
  [(get-attr Σ_1 (ref l_ins) string_key)
   (Σ_2 ☠)
   (where (l_cls g_ins) (lookup-Σ Σ_1 l_ins))
   (where (Σ_2 ☠) (get-attr Σ_1 (ref l_cls) string_key))]
  [(get-attr Σ_1 (ref l_ins) string_key)
   (Σ_3 (ref l_insval))
   (where (l_cls g_ins) (lookup-Σ Σ_1 l_ins))
   (where (Σ_2 (ref l_clsval)) (get-attr Σ_1 (ref l_cls) string_key))
   (where (Σ_3 l_insval) (alloc Σ_2 ("method" (method l_clsval l_ins))))])

(module+ test
  (define-rule (test-e-->e source target)
    (test-match SP-dynamics
                ((Σ ρ target))
                (apply-reduction-relation* e→e (term ((base-Σ)
                                                      (base-ρ)
                                                      (get-e
                                                       (compile-e (base-Ψ)
                                                                  (base-Γ)
                                                                  (base-Γ)
                                                                  (desugar-e source))))))))
  (test-e-->e 2
              (ref (con 2)))
  (test-e-->e (bin-op + 2 3)
              (ref (con 5)))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) 2))
            (term ((base-Σ) (base-ρ) (ref (con 2)))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) int))
            (term ((base-Σ) (base-ρ) (ref "int"))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (tuple 2 "foo")))
            (term ((extend (base-Σ)
                           [8 ("tuple" (tuple (ref (con 2))
                                              (ref (con "foo"))))])
                   (base-ρ)
                   (ref 8))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (set 2 "foo")))
            (term ((extend (base-Σ)
                           [8 ("set" (set (ref (con 2))
                                          (ref (con "foo"))))])
                   (base-ρ)
                   (ref 8))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (dict [2 "foo"] ["bar" 3])))
            (term ((extend (base-Σ)
                           [8 ("dict"
                               (dict [(ref (con 2)) (ref (con "foo"))]
                                     [(ref (con "bar")) (ref (con 3))]))])
                   (base-ρ) (ref 8))))
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
            (term ((base-Σ) (base-ρ) (dynamic-attribute int "__add__")))
            (term ((base-Σ) (base-ρ) (ref (attribute "float" "__add__"))))))
(define-metafunction SP-dynamics
  do-if : h e- e- -> e-
  [(do-if h_cnd e-_thn e-_els)
   e-_els
   (where #t (falsy h_cnd))]
  [(do-if h_cnd e-_thn e-_els)
   e-_thn])
(define-metafunction SP-dynamics
  do-app : Σ v v ... -> (Σ e-)

  ;; method calls are understood as function calls
  [(do-app Σ (ref l_fun) v_arg ...)
   (Σ ((ref l_mth) (ref l_slf) v_arg ...))
   (where ("method" (method l_mth l_slf))
          (lookup-Σ Σ l_fun))]

  ;; arity error
  [(do-app Σ (ref l_fun) v_arg ...)
   (Σ ☠)
   (where ("function" (def ρ ([x_arg T_arg] ...) (x_lcl ...) s-))
          (lookup-Σ Σ l_fun))
   (where #f (= (len (v_arg ...)) (len (x_arg ...))))]

  ;; function call
  [(do-app Σ_1 (ref l_fun) v_arg ...)
   (Σ_2
    (enter ρ_2 (begin
                 (define/assign x_arg v_arg)
                 ...
                 (compile-check T_arg x_arg)
                 ...
                 s-)))
   (where ("function" (def ρ_1 ([x_arg T_arg] ...) (x_lcl ...) s-))
          (lookup-Σ Σ_1 l_fun))
   (where (Σ_2 ρ_2) (declare Σ_1 ρ_1 x_lcl ...))]

  ;; primitive operators
  [(do-app Σ_1 (ref l_prc) v_arg ...)
   (Σ_2 e-_ret)
   (where ("primitive_operator" (prim-op builtin)) (lookup-Σ Σ_1 l_prc))
   (where (Σ_2 e-_ret) (delta Σ_1 builtin v_arg ...))]

  ;; create class instances, some primitive classes are handled in an ad hoc way
  [(do-app Σ (ref "int") (ref (con string)))
   (Σ (ref (con ,(string->number (term string)))))]
  [(do-app Σ (ref "str") (ref (con number)))
   (Σ (ref (con ,(format "~a" (term number)))))]
  [(do-app Σ_1 (ref l_prc) v_arg ...)
   (Σ_2 (let ([tmp ((dynamic-attribute (ref l_ins) "__init__")
                    v_arg ...)])
          (return (ref l_ins))))
   (where ("type" _) (lookup-Σ Σ_1 l_prc))
   (where (Σ_2 l_ins) (alloc Σ_1 (l_prc ())))])




(define s→s
  (reduction-relation
   SP-dynamics
   #:domain (Σ ρ s-)
   [--> (in-hole (Σ ρ S) ☠)
        (Σ ρ (error))
        "error"]
   [--> (in-hole (Σ_1 ρ_1 S) e-_1)
        (in-hole (Σ_2 ρ_2 S) e-_2)
        (where ((Σ_2 ρ_2 e-_2)) ,(apply-reduction-relation e→e (term (Σ_1 ρ_1 e-_1))))
        "e→e"]
   [--> (in-hole (Σ ρ S) (begin (expr v_any) ... (return v_ret) s- ...))
        (in-hole (Σ ρ S) (return v_ret))
        "return"]
   [--> (in-hole (Σ_1 ρ S) (define/assign x v))
        (in-hole (Σ_2 ρ S) (begin))
        (where Σ_2 (update Σ_1 [(lookup ρ x) v]))
        "define/assign x"]
   [--> (in-hole (Σ_1 ρ S) (def x_fun ([x_arg T_arg] ...) d- s-))
        (in-hole (Σ_3 ρ S) (begin))
        (where (Σ_2 l_fun) (alloc Σ_1 ("function" (def ρ ([x_arg T_arg] ...) d- s-))))
        (where l_x (lookup ρ x_fun))
        (where Σ_3 (update Σ_2 [l_x (ref l_fun)]))
        "def"]
   [--> (in-hole (Σ_1 ρ S) (class x ((ref l) ...) m- ...))
        (in-hole (Σ_3 ρ S) (begin))
        (where (Σ_2 l_fun) (alloc Σ_1 ("type" (class (l ...) ()))))
        (where l_x (lookup ρ x))
        (where Σ_3 (update Σ_2 [l_x (ref l_fun)]))
        "class"]
   [--> (in-hole (Σ ρ S) (if (ref l) s-_thn s-_els))
        (in-hole (Σ ρ S) s-_thn)
        (where #f (falsy (lookup-Σ Σ l)))
        "if truthy"]
   [--> (in-hole (Σ ρ S) (if (ref l) s-_thn s-_els))
        (in-hole (Σ ρ S) s-_els)
        (where #t (falsy (lookup-Σ Σ l)))
        "if falsy"]
   [--> (in-hole (Σ ρ S) (assert (ref l)))
        (in-hole (Σ ρ S) (begin))
        (where #f (falsy (lookup-Σ Σ l)))
        "assert-truthy"]
   [--> (in-hole (Σ ρ S) (assert (ref l)))
        (Σ ρ (error))
        (where #t (falsy (lookup-Σ Σ l)))
        "assert-falsy"]
   [--> (in-hole (Σ ρ S) (begin (expr v_1) ... (begin (expr v_2) ...) s-_2 ...))
        (in-hole (Σ ρ S) (begin (expr v_1) ... (expr v_2) ... s-_2 ...))
        "flatten begin"]))


(module+ test
  (test-match SP-dynamics (begin (expr v) ...)
              (term (calc (compile-program
                           (desugar-program
                            ())))))
  (test-match SP-dynamics (begin (expr v) ...)
              (term (calc (compile-program
                           (desugar-program
                            ((define/assign x int 42)))))))
  (test-match SP-dynamics (begin (expr v) ...)
              (term (calc (compile-program
                           (desugar-program
                            ((import-from "__static__" ("PyDict"))))))))
  ;;TODO enable the remaining tests
  #;
  (test-equal (term
               (calc ((def f ([x int]) int
                        (return (bin-op + x 1)))
                      (expr (f 2)))))
              (term
               (begin
                 (expr (ref (con 3))))))
  #;
  (test-equal (term
               (calc ((class C ())
                      (define/assign c C (C)))))
              (term
               (begin)))
  #; ;; TODO enable this test
  (test-equal (term
               (calc ((class C ()
                        (method "m1" self ([x int]) int
                                (return (bin-op + x 1))))
                      (define/assign c C (C))
                      (define/assign f dynamic (attribute c "m1"))
                      (expr (f 2)))))
              (term
               (begin
                 (expr (ref (con 3)))))))

(define-metafunction SP-dynamics
  run-imports : Σ ρ import-type ... -> (Σ ρ)
  [(run-imports Σ ρ) (Σ ρ)]
  [(run-imports Σ_1 ρ_1 (import-from string ()) import-type ...)
   (run-imports Σ_1 ρ_1 import-type ...)]
  [(run-imports Σ_1 ρ_1 (import-from "__static__" ("PyDict" any ...)) import-type ...)
   (run-imports Σ_2 ρ_2 (import-from "__static__" (any ...)) import-type ...)
   (where (Σ_2 l) (alloc Σ_1 (ref "dict")))
   (where ρ_2 (extend ρ_1 [PyDict l]))]
  [(run-imports Σ_1 ρ_1 (import-from "__static__" ("CheckedDict" any ...)) import-type ...)
   (run-imports Σ_2 ρ_2 (import-from "__static__" (any ...)) import-type ...)
   (where (Σ_2 l) (alloc Σ_1 (ref "CheckedDict")))
   (where ρ_2 (extend ρ_1 [CheckedDict l]))]
  ;; ignore inline. It is fine
  [(run-imports Σ ρ (import-from "__static__" ("inline" any ...)) import-type ...)
   (run-imports Σ ρ (import-from "__static__" (any ...)) import-type ...)]
  [(run-imports Σ_1 ρ_1 (import-from "typing" ("Optional" any ...)) import-type ...)
   (run-imports Σ_2 ρ_2 (import-from "typing" (any ...)) import-type ...)
   (where (Σ_2 l) (alloc Σ_1 (ref "Optional")))
   (where ρ_2 (extend ρ_1 [Optional l]))])

(define-metafunction SP-dynamics
  calc : program- -> s-
  [(calc (import-type ... (x ...) s-_1))
   s-_2
   (where (Σ_1 ρ_1) ((base-Σ) (base-ρ)))
   (where (Σ_2 ρ_2) (run-imports Σ_1 ρ_1 import-type ...))
   (where (Σ_3 ρ_3) (declare Σ_2 ρ_2 x ...))
   (where ((Σ_4 ρ_4 s-_2)) ,(apply-reduction-relation* s→s (term (Σ_3 ρ_3 s-_1))))]
  [(calc (import-type ... (x ...) s-_1))
   s-_2
   (where (Σ_1 ρ_1) ((base-Σ) (base-ρ)))
   (where (Σ_2 ρ_2) (run-imports Σ_1 ρ_1 import-type ...))
   (where (Σ_3 ρ_3) (declare Σ_2 ρ_2 x ...))
   (where any_more-than-one?? ,(apply-reduction-relation* s→s (term (Σ_3 ρ_3 s-_1))))
   (judgment-holds (show any_more-than-one??))])

(define-metafunction SP-dynamics
  trace-calc : program- -> s-
  [(trace-calc (import-type ... (x ...) s-_1))
   s-_2
   (where (Σ_1 ρ_1) ((base-Σ) (base-ρ)))
   (where (Σ_2 ρ_2) (run-imports Σ_1 ρ_1 import-type ...))
   (where (Σ_3 ρ_3) (declare Σ_2 ρ_2 x ...))
   (where ((Σ_4 ρ_4 s-_2)) ,(traces s→s (term (Σ_3 ρ_3 s-_1))))])

(module+ test
  (test-equal (term (declare () () a b c))
              (term (([2 ☠]
                      [1 ☠]
                      [0 ☠])
                     ([c 2]
                      [b 1]
                      [a 0])))))
(define-metafunction SP-dynamics
  declare : Σ ρ x ... -> (Σ ρ)
  [(declare Σ ρ) (Σ ρ)]
  [(declare Σ_1 ρ_1 x_1 x_2 ...)
   (declare Σ_2 ρ_2 x_2 ...)
   (where (Σ_2 l_1) (alloc Σ_1 ☠))
   (where ρ_2 (extend ρ_1 [x_1 l_1]))])

(test-match
 SP-dynamics
 (begin (expr v) ...)
 (term
  (calc
   (compile-program
    (desugar-program
     ((import-from "__static__" ("PyDict" "CheckedDict"))
      (define/assign x (subscript CheckedDict (tuple str int))
        ((subscript CheckedDict (tuple str int))
         (dict ("foo" 1))))
      (delete (subscript x "foo"))))))))
;
;(define-judgment-form SP-dynamics
;  #:mode (terminate I)
;  #:contract (terminate s-)
;
;  [-------------------------
;   (terminate (begin (expr v-) ...))])
;
;(define-judgment-form SP-dynamics
;  #:mode (error I)
;  #:contract (error s-)
;
;  [-------------------------
;   (error (expr (error)))])
|#
|#