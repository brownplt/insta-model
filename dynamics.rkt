#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "desugar.rkt")
(require "statics.rkt")
(require "compile.rkt")
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-compiled
  ;; environments maps variables to heap labels, which map to values
  ;;   the indirection is necessary because varaibles are mutable
  (ρ ([x number] ...))
  ;; heaps map addresses (heap labels) to values
  (Σ ([number h+☠] ...))
  ;; heap labels
  (l number
     ;; special heap addresses reserved by constants
     (con c)
     ;; other primitive values, e.g. builtin methods
     string)
  ;; heap values
  ;; According to Python's Data model: "every object has an identity,
  ;; a type and a value"
  ;; We don't need to store the identity here -- the heap Σ already did it.
  ;; But we still need a type (l), and an object value (g).
  (h (l g)
     ;; The v case is only used to represent (mutable) variables
     v)
  (h+☠ h ☠)
  ;; "Values" in the sense of Python's Data model. They are *before*
  ;; heap values (h), hence g
  (g c
     (tuple-syntax v ...)
     (set-syntax v ...)
     (dict-syntax (v v) ...)
     (def ρ (x ...) d- s)
     (class (l ...) ([string l] ...) ...)
     ;; the string case is for builtin_function_or_method. We don't really
     ;; need this because c includes string
     string
     ;; methods, the first l is the method itself,
     ;; the second l is the self object
     (l l))
  ;; values are just heap addresses
  (v (ref l))
  ;; results are values or (error)
  (r v ☠)
  ;; at runtime immediate values can go into expressions.
  (e- .... v ☠
      (let ([x e]) e-)
      (escape ρ s-))
  (s- .... (error))
  ;; expression contexts
  (E hole
     (tuple-syntax v ... E e- ...)
     (set-syntax v ... E e- ...)
     (dict-syntax [v v] ... [E e-] [e- e-] ...)
     (dict-syntax [v v] ... [v E] [e- e-] ...)
     (is E e-)
     (is v E)
     (is-not E e-)
     (is-not v E)
     (if E e- e-)
     (dynamic-attribute E string)
     (static-attribute E string)
     (v ... E e- ...)
     (reveal-type any ... E)
     (let ([x E]) e-)
     (check-isinstance! E e-)
     (check-isinstance! v E)
     (escape ρ S))
  ;; statement contexts
  (S hole
     (define/assign x E)
     (define/assign (attribute E string) e-)
     (define/assign (attribute v string) E)
     ;; defs are handled immediatly
     (class x (v ... E e- ...) m ...)
     (if E (s- ...) (s- ...))
     (begin (expr v) ... S s- ...)
     (delete (attribute E string))
     (return E)
     (expr E)
     (claim x E)
     (assert E))
  (Program (import-type ... d- S))
  )

(define-metafunction SP-dynamics
  alloc : Σ h+☠ -> (Σ l)
  [(alloc Σ h+☠)
   ((extend Σ [l h+☠]) l)
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
             (["__gt__" "float.__gt__"]
              ["__eq__" "float.__eq__"]
              ["__neq__" "float.__neq__"]
              ["__add__" "float.__add__"])))]
  [(lookup-Σ Σ "int")
   ("type" (class ("float") ()))]
  [(lookup-Σ Σ "bool")
   ("type" (class ("int") ()))]
  [(lookup-Σ Σ "str")
   ("type" (class ("object") ()))]
  [(lookup-Σ Σ "tuple")
   ("type" (class ("object") ()))]
  [(lookup-Σ Σ "set")
   ("type" (class ("object") ()))]
  [(lookup-Σ Σ "dict")
   ("type" (class ("object")
             (["__setitem__" "dict.__setitem__"]
              ["__getitem__" "dict.__getitem__"]
              ["__delitem__" "dict.__delitem__"])))]
  [(lookup-Σ Σ "object")
   ("type" (class () (["__new__" "object.__new__"]
                      ["__init__" "object.__init__"])))]
  [(lookup-Σ Σ "type")
   ("type" (class ("object") ()))]
  [(lookup-Σ Σ string)
   ("primitive_operator" string)])

(module+ test
  (test-equal (term (delta (base-Σ) "isinstance" (ref (con 23)) (ref "int")))
              (term ((base-Σ) (ref (con #t))))))
(define-metafunction SP-dynamics
  delta : Σ string v ... -> (Σ r)
  [(delta Σ "object.__new__" (ref "int") (ref (con string)))
   (Σ (ref (con ,(string->number (term string)))))]
  [(delta Σ "object.__new__" (ref "str") (ref (con number)))
   (Σ (ref (con ,(format "~a" (term number)))))]
  [(delta Σ_1 "object.__new__" (ref number_1) (ref number_2))
   (Σ_2 (ref l))
   (where (Σ_2 l) (alloc Σ_1 (number_1 number_2)))]
  [(delta Σ "object.__init__" v_slf v_arg ...)
   (Σ v_slf)]
  [(delta Σ "float.__add__" (ref (con number_1)) (ref (con number_2)))
   (Σ (ref (con ,(+ (term number_1) (term number_2)))))]
  [(delta Σ "dict.__getitem__" (ref l_map) v_key)
   (Σ v_val)
   (where ("dict" (dict-syntax any_1 ... [v_key v_val] any_2 ...))
          (lookup-Σ Σ l_map))]
  [(delta Σ_1 "dict.__setitem__" (ref l_map) v_key v_new)
   (Σ_2 (ref (con None)))
   (where ("dict" (dict-syntax any_1 ... [v_key v_old] any_2 ...))
          (lookup-Σ Σ_1 l_map))
   (where Σ_2 (update Σ_1 [l_map ("dict" (dict-syntax any_1 ... [v_key v_new] any_2 ...))]))]
  [(delta Σ_1 "dict.__setitem__" (ref l_map) v_key v_val)
   (Σ_2 (ref (con None)))
   (where ("dict" (dict-syntax any ...))
          (lookup-Σ Σ_1 l_map))
   (where Σ_2 (update Σ_1 [l_map ("dict" (dict-syntax [v_key v_val] any ...))]))]
  [(delta Σ_1 "dict.__delitem__" (ref l_map) v_key)
   (Σ_2 (ref (con None)))
   (where ("dict" (dict-syntax any_1 ... [v_key v_val] any_2 ...))
          (lookup-Σ Σ_1 l_map))
   (where Σ_2 (update Σ_1 [l_map ("dict" (dict-syntax any_1 ... any_2 ...))]))]
  [(delta Σ "isinstance" (ref l_ins) (ref l_tgt))
   (Σ (ref (con (issubclass Σ l_src l_tgt))))
   (where (l_src _) (lookup-Σ Σ l_ins))]
  [(delta Σ string v ...)
   (Σ ☠)])

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
;; TODO why did I say this?
(module+ test
  (test-equal (term (get-attr (base-Σ) (ref "float") "__add__"))
              (term ((base-Σ) (ref "float.__add__"))))
  (test-equal (term (get-attr (base-Σ) (ref "bool") "__add__"))
              (term ((base-Σ) (ref "float.__add__")))))
(define-metafunction SP-dynamics
  get-attr : Σ v string -> (Σ r)
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
   (where (Σ_3 l_insval) (alloc Σ_2 ("method" (l_clsval l_ins))))])

(module+ test
  (define-syntax-rule (test-e-->e source target)
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
            (term ((base-Σ) (base-ρ) (tuple-syntax 2 "foo")))
            (term ((extend (base-Σ)
                           [8 ("tuple" (tuple-syntax (ref (con 2))
                                                     (ref (con "foo"))))])
                   (base-ρ)
                   (ref 8))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (set-syntax 2 "foo")))
            (term ((extend (base-Σ)
                           [8 ("set" (set-syntax (ref (con 2))
                                                 (ref (con "foo"))))])
                   (base-ρ)
                   (ref 8))))
  (test-->> e→e
            (term ((base-Σ) (base-ρ) (dict-syntax [2 "foo"] ["bar" 3])))
            (term ((extend (base-Σ)
                           [8 ("dict"
                               (dict-syntax [(ref (con 2)) (ref (con "foo"))]
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
            (term ((base-Σ) (base-ρ) (ref "float.__add__")))))

(define e→e
  (reduction-relation
   SP-dynamics
   #:domain (Σ ρ e-)
   (--> (in-hole (Σ ρ E) c)
        (in-hole (Σ ρ E) (ref (con c)))
        "constant")
   (--> (in-hole (Σ ρ E) x)
        (in-hole (Σ ρ E) (lookup Σ l))
        (where l (lookup ρ x))
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
   (--> (in-hole (Σ ρ E) (if (ref l) e-_thn e-_els))
        (in-hole (Σ ρ E) e-_thn)
        (where h (lookup-Σ Σ l))
        (where #f (falsy h))
        "if truthy")
   (--> (in-hole (Σ ρ E) (if (ref l) e-_thn e-_els))
        (in-hole (Σ ρ E) e-_els)
        (where h (lookup-Σ Σ l))
        (where #t (falsy h))
        "if falsy")
   (--> (in-hole (Σ_1 ρ E) (dynamic-attribute v_map string))
        (in-hole (Σ_2 ρ E) r_val)
        (where (Σ_2 r_val) (get-attr Σ_1 v_map string))
        "dynamic-attribute")
   (--> (in-hole (Σ_1 ρ_1 E) ((ref l_fun) v_arg ...))
        (in-hole (Σ_2 ρ_3 E) (escape ρ_1 (begin
                                           (define/assign x_arg v_arg)
                                           ...
                                           s)))
        (where ("function" (def ρ_2 (x_arg ...) ([x_lcl D] ...) s))
               (lookup-Σ Σ_1 l_fun))
        (where (Σ_2 ρ_3) (declare Σ_1 ρ_2 x_arg ... x_lcl ...))
        "function call")
   (--> (in-hole (Σ ρ E) ((ref l_prc) v_arg ...))
        (in-hole (Σ ρ E) (let ([x_ins ((attribute (ref l_prc) "__new__") (ref l_prc) v_arg ...)])
                           (let ([x_tmp ((attribute x_ins "__init__") x_ins v_arg ...)])
                             x_ins)))
        (where ("type" g) (lookup-Σ Σ l_prc))
        (where x_ins ,(gensym 'ins))
        (where x_tmp ,(gensym 'tmp))
        "class call")
   (--> (in-hole (Σ_1 ρ E) ((ref l_prc) v_arg ...))
        (in-hole (Σ_2 ρ E) r_ret)
        (where ("primitive_operator" string) (lookup-Σ Σ_1 l_prc))
        (where (Σ_2 r_ret) (delta Σ_1 string v_arg ...))
        "primitive_operator")
   (--> (in-hole (Σ ρ E) ((ref l_prc) v_arg ...))
        (in-hole (Σ ρ E) ((ref l_mth) (ref l_slf) v_arg ...))
        (where ("method" (l_mth l_slf)) (lookup-Σ Σ l_prc))
        "method call")
   (--> (in-hole (Σ_1 ρ_1 E) (let ([x v]) e-))
        (in-hole (Σ_2 ρ_2 E) (escape ρ_1 (return e-)))
        (where (Σ_2 l) (alloc Σ_1 v))
        (where ρ_2 (extend ρ_1 [x l]))
        "let")
   (--> (in-hole (Σ ρ_1 E) (escape ρ_2 (return v)))
        (in-hole (Σ ρ_2 E) v)
        "escape")
   [--> (in-hole (Σ_1 ρ E) (check-isinstance! v_ins v_cls))
        (in-hole (Σ_2 ρ E) v_ins)
        (where (Σ_2 (ref (con #t))) (delta Σ_1 "isinstance" v_ins v_cls))
        "check-isinstance!-true"]
   [--> (in-hole (Σ_1 ρ E) (check-isinstance! v_ins v_cls))
        (in-hole (Σ_2 ρ E) ☠)
        (where (Σ_2 (ref (con #f))) (delta Σ_1 "isinstance" v_ins v_cls))
        "check-isinstance!-false"]
   #;
   [--> (in-hole (Σ_1 ρ E) (check-isinstance! v_ins v_cls))
        (in-hole (Σ_2 ρ E) ☠)
        (where any (delta Σ_1 "isinstance" v_ins v_cls))
        "check-isinstance!-???"]
   ))


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
   [--> (in-hole (Σ_1 ρ S) (def x ([x_arg t_arg] ...) t_ret d- s-))
        (in-hole (Σ_3 ρ S) (begin))
        (where (Σ_2 l_fun) (alloc Σ_1 ("function" (def ρ (x_arg ...) d- s-))))
        (where l_x (lookup ρ x))
        (where Σ_3 (update Σ_2 [l_x (ref l_fun)]))
        "def"]
   [--> (in-hole (Σ_1 ρ S) (class x ((ref l) ...) m- ...))
        (in-hole (Σ_3 ρ S) (begin))
        (where (Σ_2 l_fun) (alloc Σ_1 ("type" (class (l ...) ()))))
        (where l_x (lookup ρ x))
        (where Σ_3 (update Σ_2 [l_x (ref l_fun)]))
        "class"]
   [--> (in-hole (Σ ρ S) (assert v))
        (in-hole (Σ ρ S) (begin))
        (where #f (falsy v))
        "assert-truthy"]
   [--> (in-hole (Σ ρ S) (assert v))
        (Σ ρ (error))
        (where #t (falsy v))
        "assert-falsy"]
   [--> (in-hole (Σ ρ S) (begin (expr v_1) ... (begin s-_1 ...) s-_2 ...))
        (in-hole (Σ ρ S) (begin (expr v_1) ... s-_1 ... s-_2 ...))
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
  calc : program- -> s-
  [(calc (import-type ... (x ...) s-_1))
   s-_2
   (where (Σ_1 ρ_1) ((base-Σ) (base-ρ)))
   (where (Σ_2 ρ_2) (declare Σ_1 ρ_1 x ...))
   (where ((Σ_3 ρ_3 s-_2)) ,(apply-reduction-relation* s→s (term (Σ_2 ρ_2 s-_1))))])

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