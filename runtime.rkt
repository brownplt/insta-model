#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "desugar.rkt")
(require "compile.rkt")
(require "utilities.rkt")
(require (rename-in (only-in racket/list append*) [append* racket-append*]))
(provide (all-defined-out))

(define-extended-language SP-dynamics SP-compiled
  ;; We extended global names to create heap labels
  (l .... number)
  ;; heaps map global names to heap allocated things
  (Σ ([l h] ...))
  ;; heap allocated things includes objects, and environments
  ;; the ☠ represents uninitialized variables
  (h object
     (env ([x v+☠] ...) l+☠))
  ;; According to Python's Data model: "every object has an identity,
  ;; a type and a value"
  ;; We don't need to store the identity here -- the heap Σ already did it.
  ;; But we still need a type (l), and an object value.
  ;; We actually need two things to represent an object values:
  ;;   - primitive values (g)
  ;;   - key-attribute map (ρ)
  (object (obj l g ρ))
  ;; primitive values in the sense of Python's Data model.
  (g (con c)
     (tuple (v ...))
     (list (v ...))
     (set (v ...))
     (dict ([v v] ...))
     ;; a closure contains an environments (l), an input checks (s-), a body (level-)
     (lambda l (x ...) s- level-)
     (class (v ...) ;; parent classes
       ;; class variable mutation checks
       ([x s-] ...)
       ;; instance variable mutation checks
       ([x s-] ...))
     ;; primitive operators
     (prim-op l)
     ;; methods
     (method l l)
     ;; placeholder for user-defined classes
     (nothing))
  ;; runtime expression involves applied functions
  (e- ....
      ;; install the new environment
      (enter l s-)
      ;; restore the old environment
      (leave l s-)
      ;; raise
      (raise e-))
  (s- ....
      ;; loop
      (loop s- s-))
  (leave-begin (return v) (raise v) break continue)
  (leave-try (return v) break continue)
  (leave-final leave-try (begin) (raise v))
  (leave-loop (return v) (raise v))
  ;; runtime representation of programs
  (p [Σ l s-]
     (error any)
     (terminate))
  ;; in expression reduce expression
  (ee hole
      (tuple (v ... ee e- ...))
      (list (v ... ee e- ...))
      (set (v ... ee e- ...))
      (dict ([v v] ... [ee e-] [e- e-] ...))
      (dict ([v v] ... [v ee] [e- e-] ...))
      (is ee e-)
      (is v ee)
      (if-exp ee e- e-)
      (attribute mode ee x)
      (invoke-function l (v ... ee e- ...))
      (invoke-method l x ee (e- ...))
      (invoke-method l x v (v ... ee e- ...))
      (call-function ee (e- ...))
      (call-function v (v ... ee e- ...))
      (class x (v ... ee e- ...) ([x s-] ...) ([x s-] ...))
      (leave l se)
      (new l (v ... ee e- ...))
      (raise ee))
  ;; in expression reduce statement
  (es (tuple (v ... es e- ...))
      (list (v ... es e- ...))
      (set (v ... es e- ...))
      (dict ([v v] ... [es e-] [e- e-] ...))
      (dict ([v v] ... [v es] [e- e-] ...))
      (is es e-)
      (is v es)
      (if-exp es e- e-)
      (attribute mode es x)
      (invoke-function l (v ... es e- ...))
      (invoke-method l x es (e- ...))
      (invoke-method l x v (v ... es e- ...))
      (call-function es (e- ...))
      (call-function v (v ... es e- ...))
      (class (v ... es e- ...) ([x s-+☠] ...))
      (leave l ss)
      (new l (v ... es e- ...))
      (raise es))
  ;; in statement reduce expression
  (se (expr ee)
      (return ee)
      (begin se s- ...)
      (if ee s- s-)
      (delete (attribute mode ee x))
      (assign x ee)
      (assign (attribute mode ee x) e-)
      (assign (attribute mode v x) ee)
      (try se e- x s- s-)
      (finally se s-)
      (raise ee)
      (loop se s-))
  ;; in statement reduce statement
  (ss hole
      (expr es)
      (return es)
      (begin ss s- ...)
      (if es s- s-)
      (delete (attribute mode es x))
      (assign x es)
      (assign (attribute mode es x) e-)
      (assign (attribute mode v x) es)
      (try ss e- x s- s-)
      (finally ss s-)
      (raise es)
      (loop ss s-))
  ;; in statement bubble up exception
  (sx (expr hole)
      (return hole)
      (begin hole s- ...)
      (if hole s- s-)
      (delete (attribute mode hole x))
      (assign x hole)
      (assign (attribute mode hole x) e-)
      (assign (attribute mode v x) hole)
      (raise hole)
      (loop hole s-))
  ;; in expression bubble up exception
  (ex (tuple (v ... hole e- ...))
      (list (v ... hole e- ...))
      (set (v ... hole e- ...))
      (dict ([v v] ... [hole e-] [e- e-] ...))
      (dict ([v v] ... [v hole] [e- e-] ...))
      (is hole e-)
      (is v hole)
      (if-exp hole e- e-)
      (attribute mode hole x)
      (invoke-function l (v ... hole e- ...))
      (invoke-method l x hole (e- ...))
      (invoke-method l x v (v ... hole e- ...))
      (call-function hole (e- ...))
      (call-function v (v ... hole e- ...))
      (class (v ... hole e- ...) ([x s-+☠] ...))
      (new l (v ... hole e- ...)))
  ;; builtin-op-l, a subset of l
  (prim-op-l
   "type"
   "issubclass"
   (method "object" "__init__")
   (method "Exception" "__init__")
   (method "int" "__init__")
   (method "int" "__add__")
   (method "int" "__sub__")
   (method "int" "__mul__")
   (method "int" "__div__")
   (method "int" "__lt__")
   (method "int" "bit_length")
   (method "str" "__iter__")
   (method "str" "split")
   (method "list" "__init__")
   (method "list" "__eq__")
   (method "list" "__len__")
   (method "list" "__getitem__")
   (method "list" "__setitem__")
   (method "list" "__mul__")
   (method "list" "append")
   (method "tuple" "__init__")
   (method "tuple" "__eq__")
   (method "tuple" "__getitem__")
   (method "tuple" "__mul__")
   (method "dict" "__init__")
   (method "dict" "__getitem__")
   (method "dict" "__setitem__")
   (method "dict" "__delitem__")
   (method "dict" "__len__")
   (method "dict" "get")
   (method "dict" "items")
   (method "dict" "keys")
   (method "dict" "values")
   (method "dict" "popitem")
   (method "dict" "setdefault")
   (method "dict" "fromkeys")
   (method (chkdict T T) "__init__")
   (method (chkdict T T) "__getitem__")
   (method (chkdict T T) "__setitem__")
   (method (chkdict T T) "__delitem__")
   (method (chkdict T T) "__eq__")
   (method (chkdict T T) "clear")
   (method (chkdict T T) "get")
   (method (chkdict T T) "items")
   (method (chkdict T T) "fromkeys"))
  ;; utilities
  (v+☠ v ☠)
  (l+☠ l ☠)
  (vv [v v])
  (number+* number *))

(module+ test
  (test-equal (term (alloc ()))
              (term (())))
  (test-equal (term (alloc ()
                           (obj "int" (con 2) ())
                           (env () "builtin-env")))
              (term (([1 (env () "builtin-env")]
                      [0 (obj "int" (con 2) ())])
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
               (lookup-Σ ([0 (env () ☠)]) 0))
              (term
               (env () ☠))))
(define-metafunction SP-dynamics
  lookup-Σ : Σ l -> h
  [(lookup-Σ Σ l)
   h
   (where (yes h) (lookup? Σ l))]
  [(lookup-Σ Σ (method (user-defined-class l) x))
   (lookup-Σ Σ (lookup ρ x))
   (where (obj "type" g ρ)
          (lookup-Σ Σ (user-defined-class l)))]
  [(lookup-Σ Σ l)
   (lookup-Σ-primitive l)])
(define-metafunction SP-dynamics
  lookup-Σ-primitive : l -> h
  [(lookup-Σ-primitive "builtin-env")
   (env ([x (ref x)] ...) ☠)
   (where ([x T] ...) (base-Γ))]
  [(lookup-Σ-primitive (con c))
   (obj (l-of-c c) (con c) ())]
  [(lookup-Σ-primitive "len")
   (obj "function"
        (lambda -1 ("x")
          (begin)
          (local ("x")
            (return (call-function (attribute safe "x" "__len__") ()))))
        ())]
  [(lookup-Σ-primitive "max")
   (obj "function"
        (lambda -1 ("x" "y")
          (begin)
          (local ("x" "y")
            (if (call-function (attribute safe "y" "__lt__") ("x"))
                (return "x")
                (return "y"))))
        ())]
  [(lookup-Σ-primitive "min")
   (obj "function"
        (lambda -1 ("x" "y")
          (begin)
          (local ("x" "y")
            (if (call-function (attribute safe "y" "__lt__") ("x"))
                (return "y")
                (return "x"))))
        ())]
  [(lookup-Σ-primitive "range")
   (obj "function"
        (lambda -1 ("n")
          (begin)
          (local ("l" "i" "n")
            (begin
              (assign "l" (list ()))
              (assign "i" (con 0))
              (while (invoke-function (method "int" "__lt__") ("i" "n"))
                (begin
                  (expr (invoke-function (method "list" "append") ("l" "i")))
                  (assign "i" (invoke-function (method "int" "__add__") ("i" (con 1)))))
                (begin))
              (return "l"))))
        ())]
  [(lookup-Σ-primitive "isinstance")
   (obj "function"
        (lambda -1 ("ins" "cls")
          (begin)
          (local ("ins" "cls")
            (return (invoke-function "issubclass"
                                     ((invoke-function "type" ("ins"))
                                      "cls")))))
        ())]
  [(lookup-Σ-primitive (method "object" "__eq__"))
   (obj "function"
        (lambda -1 ("o1" "o2")
          (begin)
          (local ("o1" "o2")
            (return (is "o1" "o2"))))
        ())]
  [(lookup-Σ-primitive (method "list_iterator" "__init__"))
   (obj "function"
        (lambda -1 ("self" "seq")
          (begin)
          (local ("self" "seq")
            (begin
              (assign (attribute fast "self" "seq") "seq")
              (assign (attribute fast "self" "index") (con 0))
              (return (con None)))))
        ())]
  [(lookup-Σ-primitive (method "list_iterator" "__next__"))
   (obj "function"
        (lambda -1 ("self")
          (begin)
          (local ("self" "index" "seq" "elm")
            (begin
              (assign "index" (attribute fast "self" "index"))
              (assign "seq" (attribute fast "self" "seq"))
              (if (is "index" (call-function (attribute fast "seq" "__len__") ()))
                  (raise (new "StopIteration" ()))
                  (begin
                    (assign "elm" (call-function (attribute fast "seq" "__getitem__") ("index")))
                    (assign (attribute fast "self" "index")
                            (invoke-function (method "int" "__add__") ("index" (con 1))))
                    (return "elm"))))))
        ())]
  [(lookup-Σ-primitive (method "list" "__iter__"))
   (obj "function"
        (lambda -1 ("list")
          (begin)
          (local ("list")
            (return (new "list_iterator" ("list")))))
        ())]
  [(lookup-Σ-primitive (method "dict" "pop"))
   (obj "function"
        (lambda -1 ("dict" "key")
          (begin)
          (local ("dict" "key" "val")
            (begin
              (assign "val" (invoke-function (method "dict" "__getitem__") ("dict" "key")))
              (expr (invoke-function (method "dict" "__delitem__") ("dict" "key")))
              (return "val"))))
        ())]
  [(lookup-Σ-primitive (method "dict" "update"))
   (obj "function"
        (lambda -1 ("dict" "update")
          (begin)
          (local ("dict" "update" "items" "item" "-tmp-")
            (begin
              (assign "items"
                      (call-function
                        (attribute safe (call-function (attribute safe "update" "items") ())
                                        "__iter__")
                        ()))
              (while (con #t)
                (try
                  (begin
                    (assign "item" (call-function (attribute safe "items" "__next__") ()))
                    (expr (call-function (attribute safe "dict" "__setitem__")
                            ((call-function (attribute safe "item" "__getitem__") ((con 0)))
                             (call-function (attribute safe "item" "__getitem__") ((con 1)))))))
                  (ref "StopIteration")
                  "-tmp-"
                  break
                  (begin))
                (begin))
              (return (con None)))))
        ())]
  [(lookup-Σ-primitive (method (chkdict T_key T_val) "__delitem__"))
   (obj "function"
        (lambda -1 ("dict" "key")
          (compile-check "key" T_key)
          (local ("dict" "key")
            (begin
              (return (invoke-function (method "dict" "__delitem__") ("dict" "key"))))))
        ())]
  [(lookup-Σ-primitive (method (chkdict T_key T_val) "__getitem__"))
   (obj "function"
        (lambda -1 ("dict" "key")
          (compile-check "key" T_key)
          (local ("dict" "key")
            (begin
              (return (invoke-function (method "dict" "__getitem__") ("dict" "key"))))))
        ())]
  [(lookup-Σ-primitive (method (chkdict T_key T_val) "__setitem__"))
   (obj "function"
        (lambda -1 ("dict" "key" "val")
          (begin
            (compile-check "key" T_key)
            (compile-check "val" T_val))
          (local ("dict" "key" "val")
            (begin
              (return (invoke-function (method "dict" "__setitem__") ("dict" "key" "val"))))))
        ())]
  [(lookup-Σ-primitive (method (chkdict T_key T_val) "__setitem__"))
   (obj "function"
        (lambda -1 ("dict" "key" "val")
          (begin
            (compile-check "key" T_key)
            (compile-check "val" T_val))
          (local ("dict" "key" "val")
            (begin
              (return (invoke-function (method "dict" "__setitem__") ("dict" "key" "val"))))))
        ())]
  [(lookup-Σ-primitive (method (chkdict T_key T_val) "pop"))
   (obj "function"
        (lambda -1 ("dict" "key")
          (compile-check "key" T_key)
          (local ("dict" "key")
            (begin
              (return (invoke-function (method "dict" "pop") ("dict" "key"))))))
        ())]
  [(lookup-Σ-primitive (method (chkdict T_key T_val) "setdefault"))
   (obj "function"
        (lambda -1 ("dict" "key" "val")
          (begin 
            (compile-check "key" T_key)
            (compile-check "val" T_val))
          (local ("dict" "key" "val")
            (begin
              (return (invoke-function (method "dict" "setdefault") ("dict" "key" "val"))))))
        ())]
  [(lookup-Σ-primitive class-l)
   (obj "type"
        (class ((ref l_sup) ...)
          ([x_cmem (compile-check x_cmem T_cmem)] ...)
          ([x_imem (compile-check x_imem T_imem)] ...))
        ([x_cmem l_cmem] ...))
   (where (class (l_sup ...)
            ([x_cmem T_cmem] ...)
            ([x_cmem l_cmem] ...)
            ([x_imem T_imem] ...))
          (lookup-builtin-class class-l))]
  [(lookup-Σ-primitive prim-op-l)
   (obj "function" (prim-op prim-op-l) ())])

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
  update-env : Σ l x v+☠ -> Σ
  ;; if x is in the current environment
  [(update-env Σ l x v+☠_new)
   (update Σ [l (env (any_1 ... [x v+☠_new] any_2 ...) l+☠_out)])
   (where (env (any_1 ... [x v+☠_old] any_2 ...) l+☠_out) (lookup-Σ Σ l))]
  [(update-env Σ l x v)
   (update-env Σ l_out x v)
   (where (env any_map l_out) (lookup-Σ Σ l))])

(module+ test
  (test-equal (term (lookup-env ([0 (env (["x" (ref "int")]) ☠)])
                                0
                                "x"))
              (term (ref "int")))
  (test-equal (term (lookup-env ([1 (env () 0)]
                                 [0 (env (["x" (ref "int")]) ☠)])
                                1
                                "x"))
              (term (ref "int"))))
(define-metafunction SP-dynamics
  lookup-env : Σ l x -> v
  [(lookup-env Σ l x)
   (lookup (any_1 ... [x v+☠] any_2 ...) x)
   (where (env (any_1 ... [x v+☠] any_2 ...) l+☠_out) (lookup-Σ Σ l))]
  [(lookup-env Σ l x)
   (lookup-env Σ l_out x)
   (where (env any_map l_out) (lookup-Σ Σ l))])


(module+ test
  (test-equal (term (load (local ("i") (assign "i" (con 42)))))
              (term [([0 (env (["i" ☠]) "builtin-env")])
                     0
                     (assign "i" (con 42))])))
(define-metafunction SP-dynamics
  load : program- -> [Σ l s-]
  [(load (local (x ...) s-))
   [Σ_2 l_2 s-]
   ;(where ([x_builtin T] ...) (base-Γ))
   ;(where (Σ_1 l_1) (alloc (base-Σ) (env ([x_builtin (ref x_builtin)] ...) ☠)))
   (where (Σ_2 l_2) (alloc (base-Σ) (env ([x ☠] ...) "builtin-env")))])

(define-metafunction SP-dynamics
  delta-int-mul : Σ l ... -> [Σ e-]
  [(delta-int-mul Σ (con number_1) (con number_2))
   [Σ (ref (con ,(* (term number_1) (term number_2))))]]
  [(delta-int-mul Σ (con number) l)
   [Σ (call-function (attribute safe (ref l) "__mul__") ((ref (con number))))]])
(define-metafunction SP-dynamics
  delta-int-init : Σ l ... -> [Σ e-]
  [(delta-int-init Σ_0 l_obj (con number))
   [Σ_1 (ref (con None))]
   (where (obj l_cls g_obj ρ_obj) (lookup-Σ Σ_0 l_obj))
   (where Σ_1 (update Σ_0 [l_obj (obj l_cls (con number) ρ_obj)]))])
(define-metafunction SP-dynamics
  delta : Σ prim-op-l (v ...) -> [Σ e-]
  [(delta Σ (method "object" "__init__") (v))
   [Σ v]]
  [(delta Σ (method "Exception" "__init__") (v ...))
   (delta-exception-init Σ v ...)]
  [(delta Σ (method "int" "__init__") ((ref l_arg) ...))
   (delta-int-init Σ l_arg ...)]
  [(delta Σ (method "int" "__add__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(+ (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__sub__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(- (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__mul__") ((ref l_arg) ...))
   (delta-int-mul Σ l_arg ...)]
  [(delta Σ (method "int" "__div__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(/ (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__lt__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(< (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__eq__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(= (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "bit_length") (v ...))
   (delta-int-bit-length Σ v ...)]
  [(delta Σ (method "str" "__iter__") (v ...))
   (delta-str-iter Σ v ...)]
  [(delta Σ (method "str" "split") (v ...))
   (delta-str-split Σ v ...)]
  [(delta Σ (method "list" "__init__") (v ...))
   (delta-list-init Σ v ...)]
  [(delta Σ (method "list" "__eq__") (v ...))
   (delta-list-eq Σ v ...)]
  [(delta Σ (method "list" "__mul__") (v ...))
   (delta-list-mul Σ v ...)]
  [(delta Σ (method "list" "__len__") (v ...))
   (delta-list-len Σ v ...)]
  [(delta Σ (method "list" "__getitem__") (v ...))
   (delta-list-getitem Σ v ...)]
  [(delta Σ (method "list" "__setitem__") (v ...))
   (delta-list-setitem Σ v ...)]
  [(delta Σ (method "tuple" "__getitem__") (v ...))
   (delta-tuple-getitem Σ v ...)]
  [(delta Σ (method "list" "append") (v ...))
   (delta-list-append Σ v ...)]
  [(delta Σ (method "tuple" "__init__") (v ...))
   (delta-tuple-init Σ v ...)]
  [(delta Σ (method "tuple" "__eq__") (v ...))
   (delta-tuple-eq Σ v ...)]
  [(delta Σ (method "tuple" "__mul__") (v ...))
   (delta-tuple-mul Σ v ...)]
  [(delta Σ (method "dict" "__delitem__") (v ...))
   (delta-dict-delitem Σ v ...)]
  [(delta Σ (method "dict" "__getitem__") (v ...))
   (delta-dict-getitem Σ v ...)]
  [(delta Σ (method "dict" "__setitem__") (v ...))
   (delta-dict-setitem Σ v ...)]
  [(delta Σ (method "dict" "__len__") (v ...))
   (delta-dict-len Σ v ...)]
  [(delta Σ (method "dict" "get") ((ref l_arg) ...))
   (delta-dict-get Σ l_arg ...)]
  [(delta Σ (method "dict" "fromkeys") ((ref l_arg) ...))
   (delta-dict-fromkeys Σ l_arg ...)]
  [(delta Σ (method "dict" "keys") ((ref l_arg) ...))
   (delta-dict-keys Σ l_arg ...)]
  [(delta Σ (method "dict" "values") ((ref l_arg) ...))
   (delta-dict-values Σ l_arg ...)]
  [(delta Σ (method "dict" "items") ((ref l_arg) ...))
   (delta-dict-items Σ l_arg ...)]
  [(delta Σ (method "dict" "popitem") ((ref l_arg) ...))
   (delta-dict-popitem Σ l_arg ...)]
  [(delta Σ (method "dict" "setdefault") ((ref l_arg) ...))
   (delta-dict-setdefault Σ l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "__init__") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-init Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "clear") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-clear Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "__eq__") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-eq Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "get") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-get Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "fromkeys") ((ref l_arg) ...))
   (delta-checkeddict-fromkeys Σ T_key T_val l_arg ...)]
  [(delta Σ "type" (v ...))
   (delta-type Σ v ...)]
  [(delta Σ "issubclass" (v ...))
   (delta-issubclass Σ v ...)]
  ;; fall back
  [(delta Σ l (v ...))
   [Σ (raise (new "Exception" ((con ,(format "delta: ~a" (term l))))))]
   (where any ,(raise (format "delta: ~a" (term l))))])
(define-metafunction SP-dynamics
  delta-checkeddict-fromkeys : Σ T_key T_val l_arg ... -> [Σ e-]
  [(delta-checkeddict-fromkeys Σ T_key T_val l_arg ...)
   [Σ (call-function e-_fun ((invoke-function (method "dict" "fromkeys") ((ref l_arg) ...))))]
   (where e-_fun (lambda ("dict")
                   (begin)
                   (local ("dict" "chkdict")
                     (begin
                       (assign "chkdict" (new (chkdict T_key T_val) ()))
                       (expr (call-function (attribute fast "chkdict" "update") ("dict")))
                       (return "chkdict")))))])
(define-metafunction SP-dynamics
  delta-str-iter : Σ v ... -> [Σ e-]
  [(delta-str-iter Σ (ref (con string)))
   [Σ (new "list_iterator" ((list (v_elm ...))))]
   (where (any_char ...) ,(map string (string->list (term string))))
   (where (v_elm ...) ((ref (con any_char)) ...))])
(define-metafunction SP-dynamics
  delta-str-split : Σ v ... -> [Σ e-]
  [(delta-str-split Σ (ref (con string_1)) (ref (con string_2)) (ref (con number)))
   [Σ (list ((ref (con string_elm)) ...))]
   (where (string_elm ...)
          ,(letrec ([s1 (term string_1)]
                    [s2 (term string_2)]
                    [n (term number)])
             (letrec ([o (string-split s1 s2 #:trim? #f)])
               (if (>= n 0)
                   (take o (min (add1 n) (length o)))
                   o))))]
  [(delta-str-split Σ (ref (con string)) (ref (con None)) v_rst ...)
   (delta-str-split Σ (ref (con string)) (ref (con " ")) v_rst ...)]
  [(delta-str-split Σ (ref (con string_1)) (ref (con string_2)))
   (delta-str-split Σ (ref (con string_1)) (ref (con string_2)) (ref (con -1)))])
(define-metafunction SP-dynamics
  delta-exception-init : Σ v ... -> [Σ e-]
  [(delta-exception-init Σ (ref l_exn))
   (delta-exception-init Σ (ref l_exn) (ref (con "")))]
  [(delta-exception-init Σ_0 (ref l_exn) (ref l_msg))
   [Σ_1 (ref (con None))]
   (where (obj l_exncls (nothing) ()) (lookup-Σ Σ_0 l_exn))
   (where (obj l_msgcls (con string) ρ) (lookup-Σ Σ_0 l_msg))
   (where Σ_1 (update Σ_0 [l_exn (obj l_exncls (con string) ())]))]
  [(delta-exception-init Σ_0 (ref l_exn) (ref l_msg))
   [Σ_0 (raise (new "Exception" ((con "Exception: must be initialized with a string"))))]])
(define-metafunction SP-dynamics
  delta-checkeddict-clear : Σ T_key T_val l_obj l_arg ... -> [Σ e-]
  [(delta-checkeddict-clear Σ_0 T_key T_val l_obj)
   [Σ_1 (ref l_obj)]
   (where (obj l_cls g ρ)
          (lookup-Σ Σ_0 l_obj))
   (where Σ_1
          (update Σ_0 [l_obj (obj l_cls (dict ()) ρ)]))])
(define-metafunction SP-dynamics
  delta-dict-items : Σ l_obj l_arg ... -> [Σ e-]
  [(delta-dict-items Σ_0 l_obj)
   [Σ_2 (ref l_lst)]
   (where (obj l_cls (dict ([v_key v_val] ...)) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where (Σ_1 l_itm ...)
          (alloc Σ_0 (obj "tuple" (tuple (v_key v_val)) ()) ...))
   (where (Σ_2 l_lst)
          (alloc Σ_1 (obj "list" (list ((ref l_itm) ...)) ())))])
(define-metafunction SP-dynamics
  delta-dict-popitem : Σ l_obj l_arg ... -> [Σ e-]
  [(delta-dict-popitem Σ_0 l_obj)
   [Σ_2 (ref l_ret)]
   (where (obj l_cls (dict ([v_k1 v_v1] [v_k2 v_v2] ...)) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where [Σ_1 l_ret]
          (alloc Σ_0 (obj "tuple" (tuple (v_k1 v_v1)) ())))
   (where Σ_2
          (update Σ_1 [l_obj (obj l_cls (dict ([v_k2 v_v2] ...)) ρ)]))]
  [(delta-dict-popitem Σ l_obj)
   [Σ (raise (new "KeyError" ()))]])
(define-metafunction SP-dynamics
  delta-dict-setdefault : Σ l_obj l_arg ... -> [Σ e-]
  [(delta-dict-setdefault Σ l_obj l_key l_val)
   [Σ v_val]
   (where (obj l_cls (dict (vv_1 ... [(ref l_key) v_val] vv_2 ...)) ρ)
          (lookup-Σ Σ l_obj))]
  [(delta-dict-setdefault Σ_0 l_obj l_key l_val)
   [Σ_1 v_val]
   (where v_val (ref l_val))
   (where (obj l_cls (dict (vv ...)) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where Σ_1
          (update Σ_0 [l_obj (obj l_cls (dict ([(ref l_key) v_val] vv ...)) ρ)]))])
(define-metafunction SP-dynamics
  delta-dict-keys : Σ l_obj l_arg ... -> [Σ e-]
  [(delta-dict-keys Σ_0 l_obj)
   [Σ_1 (ref l_ret)]
   (where (obj l_cls (dict ([v_key v_val] ...)) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where [Σ_1 l_ret]
          (alloc Σ_0 (obj "list" (list (v_key ...)) ())))])
(define-metafunction SP-dynamics
  delta-dict-values : Σ l_obj l_arg ... -> [Σ e-]
  [(delta-dict-values Σ_0 l_obj)
   [Σ_1 (ref l_ret)]
   (where (obj l_cls (dict ([v_key v_val] ...)) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where [Σ_1 l_ret]
          (alloc Σ_0 (obj "list" (list (v_val ...)) ())))])
(define-metafunction SP-dynamics
  delta-checkeddict-keys : Σ T_key T_val l_obj l_arg ... -> [Σ e-]
  [(delta-checkeddict-keys Σ_0 T_key T_val l_obj)
   [Σ_1 (ref l_ret)]
   (where (obj l_cls (dict ([v_key v_val] ...)) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where [Σ_1 l_ret]
          (alloc Σ_0 (obj "list" (list (v_key ...)) ())))])
(define-metafunction SP-dynamics
  delta-checkeddict-get : Σ T_key T_val l_obj l_arg ... -> [Σ e-]
  [(delta-checkeddict-get Σ T_key T_val l_obj l_key)
   [Σ (enter -1
             (begin
               (compile-check (ref l_key) T_key)
               (return (invoke-function (method "dict" "get") ((ref l_obj) (ref l_key))))))]]
  [(delta-checkeddict-get Σ T_key T_val l_obj l_key l_val)
   [Σ (enter -1
             (begin
               (compile-check (ref l_key) T_key)
               (compile-check (ref l_val) T_val)
               (return (invoke-function (method "dict" "get") ((ref l_obj) (ref l_key) (ref l_val))))))]]
  [(delta-checkeddict-get Σ T_key T_val l_obj l_key l_arg ...)
   [Σ (raise (new "Exception" ((con "delta-checkeddict-get"))))]])
(define-metafunction SP-dynamics
  delta-dict-get : Σ l_obj l_arg ... -> [Σ e-]
  [(delta-dict-get Σ l_obj l_key)
   (delta-dict-get Σ l_obj l_key (con None))]
  [(delta-dict-get Σ l_obj l_key l_val)
   [Σ v_val]
   (where (obj l_typ (dict (vv_1 ... [(ref l_key) v_val] vv_2 ...)) ρ) (lookup-Σ Σ l_obj))]
  [(delta-dict-get Σ l_obj l_key l_val)
   [Σ (ref l_val)]
   (where (obj l_typ (dict (vv ...)) ρ) (lookup-Σ Σ l_obj))]
  [(delta-dict-get Σ l_obj l_key l_arg ...)
   [Σ (raise (new "Exception" ((con "delta-dict-get"))))]])
(define-metafunction SP-dynamics
  delta-dict-fromkeys : Σ l_arg ... -> [Σ e-]
  [(delta-dict-fromkeys Σ l_keys)
   (delta-dict-fromkeys Σ l_keys (con None))]
  [(delta-dict-fromkeys Σ l_keys l_val)
   [Σ (call-function e-_fun ((ref l_keys) (ref l_val)))]
   (where e-_fun (as-dyn (compile-function
                           (base-Ψ) (base-Γ)
                           (["keys" dynamic] ["default" dynamic])
                           dynamic
                           (desugar-s*-to-level
                             (["keys" dynamic] ["default" dynamic])
                             ((ann-assign "obj" dynamic (dict ()))
                              (for "key" "keys"
                                ((assign ((subscript "obj" "key")) "default"))
                                ())
                              (return "obj"))))))]
  [(delta-dict-fromkeys Σ l_keys l_arg ...)
   [Σ (raise (new "Exception" ((con "delta-dict-fromkeys"))))]])
(define-metafunction SP-dynamics
  delta-checkeddict-eq : Σ T_key T_val l_obj l_arg ... -> [Σ e-]
  [(delta-checkeddict-eq Σ T_key T_val l_obj l_arg)
   [Σ (ref (con,(equal? (term g_lft) (term g_rht))))]
   (where (obj l_lft g_lft ρ_lft)
          (lookup-Σ Σ l_obj))
   (where (obj l_rht g_rht ρ_rht)
          (lookup-Σ Σ l_arg))])
(define-metafunction SP-dynamics
  delta-checkeddict-init : Σ T_key T_val l_obj l_arg ... -> [Σ e-]
  [(delta-checkeddict-init Σ_0 T_key T_val l_obj)
   [Σ_1 (ref l_obj)]
   (where (obj (chkdict T_key T_val) (nothing) ())
          (lookup-Σ Σ_0 l_obj))
   (where Σ_1
          (update Σ_0
                  [l_obj (obj (chkdict T_key T_val)
                              (dict ())
                              ())]))]
  [(delta-checkeddict-init Σ T_key T_val l_obj l_dct)
   [Σ (call-function e-_fun ((ref l_obj) (ref l_dct)))]
   (where e-_fun (lambda ("obj" "dct")
                   (begin)
                   (local ("obj" "dct")
                     (begin
                       (expr (call-function (attribute fast "obj" "__init__") ()))
                       (expr (call-function (attribute fast "obj" "update") ("dct")))
                       (return (ref (con None)))))))]
  #;
  [(delta-checkeddict-init Σ_0 T_key T_val l_obj l_dct)
   [Σ_1 (enter -1
               ;; TODO: switch to for, because we can't know v_key and v_val at compile time
               (begin
                 (compile-check v_key T_key)
                 ...
                 (compile-check v_val T_val)
                 ...
                 (return (ref l_obj))))]
   (where (obj (chkdict T_key T_val) (nothing) ())
          (lookup-Σ Σ_0 l_obj))
   (where (obj "dict" (dict ([v_key v_val] ...)) ρ)
          (lookup-Σ Σ_0 l_dct))
   (where Σ_1
          (update Σ_0
                  [l_obj (obj (chkdict T_key T_val)
                              (dict ([v_key v_val] ...))
                              ())]))]
  [(delta-checkeddict-init Σ T_key T_val l_obj l_dct)
   [Σ (raise (new "Exception" ((con "CheckedDict expects a valid dict"))))]])
(define-metafunction SP-dynamics
  delta-checkeddict-delitem : Σ T_key T_val l_obj l_key -> [Σ e-]
  [(delta-checkeddict-delitem Σ_0 T_key T_val l_obj l_key)
   [Σ_1 (enter -1
               ;; TODO: switch to for, because we can't know v_key and v_val at compile time
               (begin
                 (compile-check (ref l_key) T_key)
                 (return (ref l_obj))))]
   (where (obj (chkdict T_key T_val) (nothing) ())
          (lookup-Σ Σ l_obj))
   (where (obj "dict" (dict ([v_key v_val] ...)) ρ)
          (lookup-Σ Σ l_dct))])
(define-metafunction SP-dynamics
  delta-dict-getitem : Σ v ... -> [Σ e-]
  [(delta-dict-getitem Σ_0 (ref l_map) v_key)
   [Σ_0 v_val]
   (where (obj l_typ (dict (vv_1 ... [v_key v_val] vv_2 ...)) ρ) (lookup-Σ Σ_0 l_map))]
  [(delta-dict-getitem Σ (ref l_map) v_key)
   [Σ (raise (new "KeyError" ()))]]
  [(delta-dict-getitem Σ v ...)
   [Σ (raise (new "Exception" ((con ,(format "getitem ~a" (term (v ...)))))))]])
(define-metafunction SP-dynamics
  delta-dict-setitem : Σ v ... -> [Σ e-]
  [(delta-dict-setitem Σ_0 (ref l_map) v_key v_new)
   [Σ_1 (ref (con None))]
   (where (obj l_typ (dict (vv_1 ... [v_key v_old] vv_2 ...)) ρ) (lookup-Σ Σ_0 l_map))
   (where Σ_1 (update Σ_0 [l_map (obj l_typ (dict (vv_1 ... [v_key v_new] vv_2 ...)) ρ)]))]
  [(delta-dict-setitem Σ_0 (ref l_map) v_key v_val)
   [Σ_1 (ref (con None))]
   (where (obj l_typ (dict (vv ...)) ρ) (lookup-Σ Σ_0 l_map))
   (where Σ_1 (update Σ_0 [l_map (obj l_typ (dict (vv ... [v_key v_val])) ρ)]))]
  [(delta-dict-setitem Σ v ...)
   [Σ (raise (new "Exception" ((con "setitem"))))]])
(define-metafunction SP-dynamics
  delta-dict-len : Σ v ... -> [Σ e-]
  [(delta-dict-len Σ (ref l_map))
   [Σ (ref (con number))]
   (where (obj l_typ (dict (vv ...)) ρ) (lookup-Σ Σ l_map))
   (where number ,(length (term (vv ...))))]
  [(delta-dict-len Σ v ...)
   [Σ (raise (new "Exception" ((con "dict.__len__"))))]])
(define-metafunction SP-dynamics
  delta-dict-delitem : Σ v ... -> [Σ e-]
  [(delta-dict-delitem Σ_0 (ref l_map) v_key)
   [Σ_1 (ref (con None))]
   (where (obj l_typ (dict (vv_1 ... [v_key v_val] vv_2 ...)) ρ) (lookup-Σ Σ_0 l_map))
   (where Σ_1 (update Σ_0 [l_map (obj l_typ (dict (vv_1 ... vv_2 ...)) ρ)]))]
  [(delta-dict-delitem Σ v ...)
   [Σ (raise (new "Exception" ((con "delitem"))))]])
(define-metafunction SP-dynamics
  delta-int-bit-length : Σ v ... -> [Σ e-]
  [(delta-int-bit-length Σ (ref (con number_1)))
   [Σ (ref (con number_2))]
   (where number_2 ,(inexact->exact (ceiling (log (+ (term number_1) 1) 2))))])
(define-metafunction SP-dynamics
  delta-list-init : Σ v ... -> [Σ e-]
  [(delta-list-init Σ_0 (ref l_obj))
   [Σ_1 (ref (con None))]
   (where (obj l_cls (nothing) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where Σ_1
          (update Σ_0 [l_obj (obj l_cls (list ()) ρ)]))]
  [(delta-list-init Σ_0 (ref l_obj) (ref l_init))
   [Σ_1 (ref (con None))]
   (where (obj l_cls (nothing) ρ)
          (lookup-Σ Σ_0 l_obj))
   (where (obj l_initcls (list (v_val ...)) ρ_init)
          (lookup-Σ Σ_0 l_init))
   (where Σ_1
          (update Σ_0 [l_obj (obj l_cls (list (v_val ...)) ρ)]))])
(define-metafunction SP-dynamics
  delta-tuple-init : Σ v ... -> [Σ e-]
  [(delta-tuple-init Σ_0 (ref l_obj) (ref l_tpl))
   [Σ_1 (ref (con None))]
   (where (obj l_objcls g_obj ρ_obj)
          (lookup-Σ Σ_0 l_obj))
   (where (obj l_tplcls g_tpl ρ_tpl)
          (lookup-Σ Σ_0 l_tpl))
   (where Σ_1
          (update Σ_0 [l_obj (obj l_objcls g_tpl ρ_obj)]))])
(define-metafunction SP-dynamics
  delta-list-eq : Σ v ... -> [Σ e-]
  [(delta-list-eq Σ (ref l_lft) (ref l_rht))
   [Σ (enter -1
             (return (make-and (call-function (attribute safe v_lft "__eq__")  (v_rht)) ...)))]
   (where (obj l_lftcls (list (v_lft ...)) ρ_lft) (lookup-Σ Σ l_lft))
   (where (obj l_rhtcls (list (v_rht ...)) ρ_rht) (lookup-Σ Σ l_rht))
   (where #t ,(= (length (term (v_lft ...))) (length (term (v_rht ...)))))]
  [(delta-list-eq Σ (ref l_lft) (ref l_rht))
   [Σ (ref (con #f))]])
(define-metafunction SP-dynamics
  delta-list-mul : Σ v ... -> [Σ e-]
  [(delta-list-mul Σ (ref l_lft) (ref (con number)))
   [Σ (list (v_new ...))]
   (where (obj l_cls (list (v_elm ...)) ρ) (lookup-Σ Σ l_lft))
   (where (v_new ...) ,(racket-append* (build-list (term number) (lambda (_) (term (v_elm ...))))))])
(define-metafunction SP-dynamics
  delta-tuple-mul : Σ v ... -> [Σ e-]
  [(delta-tuple-mul Σ (ref l_lft) (ref (con number)))
   [Σ (tuple (v_new ...))]
   (where (obj l_cls (tuple (v_elm ...)) ρ) (lookup-Σ Σ l_lft))
   (where (v_new ...) ,(racket-append* (build-list (term number) (lambda (_) (term (v_elm ...))))))])
(define-metafunction SP-dynamics
  delta-list-len : Σ v ... -> [Σ e-]
  [(delta-list-len Σ (ref l_obj))
   [Σ (con ,(length (term (v_elm ...))))]
   (where (obj l_cls (list (v_elm ...)) ρ_obj) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  delta-list-setitem : Σ v ... -> [Σ e-]
  [(delta-list-setitem Σ_0 (ref l_obj) (ref (con number)) v_new)
   [Σ_1 (ref (con None))]
   (where (obj l_cls (list (v_elm ...)) ρ) (lookup-Σ Σ_0 l_obj))
   (where #t ,(< (term number) (length (term (v_elm ...)))))
   (where (v_ret ...) ,(list-set (term (v_elm ...)) (term number) (term v_new)))
   (where Σ_1 (update Σ_0 [l_obj (obj l_cls (list (v_ret ...)) ρ)]))])
(define-metafunction SP-dynamics
  delta-list-getitem : Σ v ... -> [Σ e-]
  [(delta-list-getitem Σ (ref l_obj) (ref (con number)))
   [Σ v_ret]
   (where (obj l_cls (list (v_elm ...)) ρ) (lookup-Σ Σ l_obj))
   (where #t ,(< (term number) (length (term (v_elm ...)))))
   (where v_ret ,(list-ref (term (v_elm ...)) (term number)))])
(define-metafunction SP-dynamics
  delta-tuple-getitem : Σ v ... -> [Σ e-]
  [(delta-tuple-getitem Σ (ref l_obj) (ref (con number)))
   [Σ v_ret]
   (where (obj l_cls (tuple (v_elm ...)) ρ) (lookup-Σ Σ l_obj))
   (where #t ,(< (term number) (length (term (v_elm ...)))))
   (where v_ret ,(list-ref (term (v_elm ...)) (term number)))])
(define-metafunction SP-dynamics
  delta-list-append : Σ v ... -> [Σ e-]
  [(delta-list-append Σ_0 (ref l_lst) v_elm)
   [Σ_1 (con None)]
   (where (obj l_cls (list (v_lft ...)) ρ) (lookup-Σ Σ_0 l_lst))
   (where Σ_1 (update Σ_0 [l_lst (obj l_cls (list (v_lft ... v_elm)) ρ)]))])
(define-metafunction SP-dynamics
  delta-tuple-eq : Σ v ... -> [Σ e-]
  [(delta-tuple-eq Σ (ref l_lft) (ref l_rht))
   [Σ (enter -1
             (return (make-and (call-function (attribute safe v_lft "__eq__")  (v_rht)) ...)))]
   (where (obj l_lftcls (tuple (v_lft ...)) ρ_lft) (lookup-Σ Σ l_lft))
   (where (obj l_rhtcls (tuple (v_rht ...)) ρ_rht) (lookup-Σ Σ l_rht))
   (where #t ,(= (length (term (v_lft ...))) (length (term (v_rht ...)))))]
  [(delta-tuple-eq Σ (ref l_lft) (ref l_rht))
   [Σ (ref (con #f))]])
(define-metafunction SP-dynamics
  make-and : e- ... -> e-
  [(make-and)
   (con #t)]
  [(make-and e-_1 e-_2 ...)
   (make-and2 e-_1 (make-and e-_2 ...))])
(define-metafunction SP-dynamics
  make-and2 : e- e- -> e-
  [(make-and2 e-_1 e-_2)
   (if-exp e-_1 (con #t) e-_2)])
(define-metafunction SP-dynamics
  delta-type : Σ v ... -> [Σ e-]
  ;; same class refl
  [(delta-type Σ (ref l_obj))
   [Σ (ref l_cls)]
   (where (obj l_cls g ρ)
          (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  delta-issubclass : Σ v ... -> [Σ e-]
  ;; same class refl
  [(delta-issubclass Σ (ref l) (ref l))
   [Σ (ref (con #t))]
   (where (obj "type" (class (v ...) ([x_cmem s-_cmem] ...) ([x_imem s-_imem] ...)) ρ)
          (lookup-Σ Σ l))]
  ;; reach object
  [(delta-issubclass Σ (ref "object") (ref l))
   [Σ (ref (con #f))]
   (where (obj "type" (class (v ...) ([x_cmem s-_cmem] ...) ([x_imem s-_imem] ...)) ρ)
          (lookup-Σ Σ l))]
  ;; go up
  [(delta-issubclass Σ (ref l_1) (ref l_2))
   (delta-issubclass Σ (ref l_3) (ref l_2))
   (where (obj "type" (class ((ref l_3)) ([x_cmem s-_cmem] ...) ([x_imem s-_imem] ...)) ρ)
          (lookup-Σ Σ l_1))]
  ;; arity error
  [(delta-issubclass Σ v)
   [Σ (raise (new "Exception" ((con "issubclass: arity error"))))]]
  [(delta-issubclass Σ v_1 v_2 v_3 v_4 ...)
   [Σ (raise (new "Exception" ((con "issubclass: arity error"))))]])

(define-metafunction SP-dynamics
  do-call-function : Σ l h (v ...) -> [Σ l e-]
  ;; This function assumes nothing about the operator `h` and the arguments `(v ...)`
  [(do-call-function Σ l (obj "method" g ρ) (v ...))
   (do-call-function-good-rator Σ l (obj "method" g ρ) (v ...))]
  [(do-call-function Σ l (obj "function" g ρ) (v ...))
   (do-call-function-good-rator Σ l (obj "function" g ρ) (v ...))]
  [(do-call-function Σ l h (v ...))
   [Σ l (raise (new "Exception" ((con ,(format "CALL_FUNCTION: not a callable ~a" (term h))))))]])
(define-metafunction SP-dynamics
  do-call-function-good-rator : Σ l h (v ...) -> [Σ l e-]
  ;; This function assumes nothing about the operator `h` and the arguments `(v ...)`
  [(do-call-function-good-rator Σ l (obj "method" (method l_fun l_obj) ρ) (v ...))
   [Σ l (call-function (ref l_fun) ((ref l_obj) v ...))]]
  [(do-call-function-good-rator Σ l (obj "function" g ρ) (v ...))
   (do-call-function-check-arity Σ l (obj "function" g ρ) (v ...))])
(define-metafunction SP-dynamics
  do-call-function-check-arity : Σ l (obj "function" g ρ) (v ...) -> [Σ l e-]
  [(do-call-function-check-arity Σ l (obj "function" g ρ) (v ...))
   (do-invoke-function-safe Σ l (obj "function" g ρ) (v ...))
   (where * (arity g))]
  [(do-call-function-check-arity Σ l (obj "function" g ρ) (v ...))
   (do-invoke-function-safe Σ l (obj "function" g ρ) (v ...))
   (where #t ,(= (term (arity g)) (length (term (v ...)))))]
  [(do-call-function-check-arity Σ l (obj "function" g ρ) (v ...))
   [Σ l (enter -1
               (raise (new "Exception" ((con ,(format "arity-mismatch ~a: ~a vs ~a; ~a"
                                                      (term g)
                                                      (term (arity g))
                                                      (length (term (v ...)))
                                                      (term (v ...))))))))]])
(define-metafunction SP-dynamics
  arity : g -> number+*
  [(arity (prim-op l))
   (arity-prim-op l)]
  [(arity (lambda l_cls (x_arg ...) s-_chk (local (x_var ...) s-_bdy)))
   ,(length (term (x_arg ...)))])
(define-metafunction SP-dynamics
  arity-prim-op : prim-op-l -> number+*
  [(arity-prim-op "type") 1]
  [(arity-prim-op "isinstance") 2]
  [(arity-prim-op "issubclass") 2]
  [(arity-prim-op (method l_cls x_mth))
   ,(length (term (dynamic T_arg ...)))
   (where (-> (T_arg ...) T_out) (lookup-member-T (base-Ψ) l_cls x_mth))]
  [(arity-prim-op (method l_cls x_mth))
   *
   (where dynamic (lookup-member-T (base-Ψ) l_cls x_mth))]
  #|
  [(arity-prim-op (method "object" "__init__")) 1]
  [(arity-prim-op (method "Exception" "__init__")) *]
  [(arity-prim-op (method "int" "__add__")) 2]
  [(arity-prim-op (method "int" "__sub__")) 2]
  [(arity-prim-op (method "int" "__mul__")) 2]
  [(arity-prim-op (method "int" "__div__")) 2]
  [(arity-prim-op (method "int" "__eq__")) 2]
  [(arity-prim-op (method "dict" "__init__")) 2]
  [(arity-prim-op (method "dict" "__getitem__")) 2]
  [(arity-prim-op (method "dict" "__setitem__")) 3]
  [(arity-prim-op (method "dict" "__delitem__")) 2]
  [(arity-prim-op (method "dict" "get")) *]
  [(arity-prim-op (method (chkdict T_key T_val) "__init__")) *]
  [(arity-prim-op (method (chkdict T_key T_val) "__getitem__")) 2]
  [(arity-prim-op (method (chkdict T_key T_val) "__setitem__")) 3]
  [(arity-prim-op (method (chkdict T_key T_val) "__delitem__")) 2]
  [(arity-prim-op (method (chkdict T_key T_val) "__eq__")) 1]
  [(arity-prim-op (method (chkdict T_key T_val) "get")) *]
  |#)

(define-metafunction SP-dynamics
  do-invoke-method : Σ l l x v (v ...) -> [Σ l e-]
  ;; This function assumes that `v` is an instance of (a subclass of) `l`,
  ;;   and arguments `(v ...)` are well-typed.
  ;; invoke the method `x` declared in class `l`.
  [(do-invoke-method Σ l_env l_cls x v_obj (v_arg ...))
   [Σ l_env (call-function (attribute fast v_obj x) (v_arg ...))]])

(define-metafunction SP-dynamics
  do-invoke-function-safe : Σ l h (v ...) -> [Σ l e-]
  ;; invoke the function `h` under the environment `l`.
  ;; This function assumes that `h` is an instance of `function`
  ;;   and arguments `(v ...)` are well-typed.
  ;; primitive function
  [(do-invoke-function-safe Σ l_env (obj "function" (prim-op l) ()) (v ...))
   [Σ_1 l_env e-]
   (where [Σ_1 e-] (delta Σ l (v ...)))]
  ;; derived function
  [(do-invoke-function-safe Σ l_env
                       (obj "function"
                            (lambda l_cls (x_arg ...)
                              s-_chk
                              (local (x_var ...) s-_bdy))
                            ρ)
                       (v_arg ...))
   (do-invoke-function-function-safe Σ l_env l_cls (x_arg ...) s-_chk (x_var ...) s-_bdy ρ (v_arg ...))]
  ;; the type function
  [(do-invoke-function-safe Σ l_env h ((ref l_arg)))
   [Σ l_env (ref l_cls)]
   (where h (lookup-Σ Σ "type"))
   (where (obj l_cls g ρ) (lookup-Σ Σ l_arg))])
(define-metafunction SP-dynamics
  do-invoke-function-fast : Σ l h (v ...) -> [Σ l e-]
  ;; invoke the function `h` under the environment `l`.
  ;; This function assumes that `h` is an instance of `function`
  ;;   and arguments `(v ...)` are well-typed.
  ;; primitive function
  [(do-invoke-function-fast Σ l_env (obj "function" (prim-op l) ()) (v ...))
   [Σ_1 l_env e-]
   (where [Σ_1 e-] (delta Σ l (v ...)))]
  ;; derived function
  [(do-invoke-function-fast Σ l_env
                       (obj "function"
                            (lambda l_cls (x_arg ...)
                              s-_chk
                              (local (x_var ...) s-_bdy))
                            ρ)
                       (v_arg ...))
   (do-invoke-function-function-fast Σ l_env l_cls (x_arg ...) s-_chk (x_var ...) s-_bdy ρ (v_arg ...))]
  ;; the type function
  [(do-invoke-function-fast Σ l_env h ((ref l_arg)))
   [Σ l_env (ref l_cls)]
   (where h (lookup-Σ Σ "type"))
   (where (obj l_cls g ρ) (lookup-Σ Σ l_arg))])
(define-metafunction SP-dynamics
  do-invoke-function-function-safe : Σ l l (x ...) s- (x ...) s- ρ (v ...) -> [Σ l e-]
  [(do-invoke-function-function-safe Σ_0 l_env l_cls (x_arg ...) s-_chk (x_var ...) s-_bdy ρ (v_arg ...))
   [Σ_1
    l_bdy
    (leave l_env
           (begin
             (assign x_arg v_arg)
             ...
             s-_chk
             s-_bdy))]
   (where #t ,(= (length (term (x_arg ...)))
                 (length (term (v_arg ...)))))
   (where (Σ_1 l_bdy) (alloc Σ_0 (env ([x_var ☠] ...) l_cls)))])
(define-metafunction SP-dynamics
  do-invoke-function-function-fast : Σ l l (x ...) s- (x ...) s- ρ (v ...) -> [Σ l e-]
  [(do-invoke-function-function-fast Σ_0 l_env l_cls (x_arg ...) s-_chk (x_var ...) s-_bdy ρ (v_arg ...))
   [Σ_1
    l_bdy
    (leave l_env
           (begin
             (assign x_arg v_arg)
             ...
             s-_bdy))]
   (where #t ,(= (length (term (x_arg ...)))
                 (length (term (v_arg ...)))))
   (where (Σ_1 l_bdy) (alloc Σ_0 (env ([x_var ☠] ...) l_cls)))])

(define-metafunction SP-dynamics
  do-attribute : Σ mode l x -> [Σ e-]
  ;; When the attribute is defined by the object
  ;;   return the corresponding attribute
  [(do-attribute Σ mode l_obj x)
   [Σ (ref l_att)]
   (where #f ,(redex-match? SP-dynamics dunder-member (term x)))
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where (yes l_att) (lookup? ρ x))]
  ;; When the attribute is defined by the class
  ;;   find the corresponding attribute
  ;;   and wrap it as necessary
  [(do-attribute Σ mode l_obj x)
   (wrap-attribute Σ l_att l_obj)
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where l_att (lookup-class-attribute Σ mode l_cls x))]
  [(do-attribute Σ mode l_obj x)
   [Σ (raise (new "AttributeError" ((con ,(format "undefined attribute ~a.~a" (term l_cls) (term x))))))]
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where ☠ (lookup-class-attribute Σ mode l_cls x))])
(define-metafunction SP-dynamics
  lookup-class-attribute : Σ mode l x -> l+☠
  [(lookup-class-attribute Σ mode "type" "__eq__")
   (method "object" "__eq__")]
  [(lookup-class-attribute Σ mode l_cls x)
   l_att
   (where (obj "type" g ρ) (lookup-Σ Σ l_cls))
   (where (yes l_att) (lookup? ρ x))]
  [(lookup-class-attribute Σ mode l_cls x)
   (lookup-class-attribute Σ mode l_prn x)
   (where (obj "type" (class ((ref l_prn)) ([x_cmem s-_cmem] ...) ([x_imem s-_imem] ...)) ρ)
          (lookup-Σ Σ l_cls))]
  [(lookup-class-attribute Σ mode l_cls x)
   ☠])
(define-metafunction SP-dynamics
  wrap-attribute : Σ l_att l_obj -> [Σ v]
  [(wrap-attribute Σ_0 l_att l_obj)
   [Σ_1 (ref l_mth)]
   (where (obj "function" g ρ) (lookup-Σ Σ_0 l_att))
   (where [Σ_1 l_mth] (alloc Σ_0 (obj "method" (method l_att l_obj) ())))]
  [(wrap-attribute Σ l_att l_obj)
   [Σ (ref l_att)]])

(define-syntax-rule (m? x y)
  (redex-match? SP-dynamics x y))

(module+ test
  (test-equal (term (do-is (ref (con 1)) (ref (con 2))))
              (term #f))
  (test-equal (term (do-is (ref (con 2)) (ref (con 2))))
              (term #t)))
(define-metafunction SP-dynamics
  do-is : v v -> boolean
  [(do-is v v) #t]
  [(do-is v_1 v_2) #f])
(module+ test
  (test-equal (term #t) (term (falsy? (obj "NoneType" (con None) ()))))
  (test-equal (term #t) (term (falsy? (obj "int" (con 0) ()))))
  (test-equal (term #t) (term (falsy? (obj "bool" (con #f) ()))))
  (test-equal (term #t) (term (falsy? (obj "tuple" (tuple ()) ()))))
  (test-equal (term #t) (term (falsy? (obj "set" (set ()) ()))))
  (test-equal (term #t) (term (falsy? (obj "dict" (dict ()) ()))))
  (test-equal (term #f) (term (falsy? (obj "int" (con 2) ()))))
  (test-equal (term #f) (term (falsy? (obj "bool" (con #t) ()))))
  (test-equal (term #f) (term (falsy? (obj "tuple" (tuple ((ref (con None)))) ())))))
(define-metafunction SP-dynamics
  falsy? : h -> boolean
  [(falsy? (obj "NoneType" (con None) ())) #t]
  [(falsy? (obj "int" (con 0) ())) #t]
  [(falsy? (obj "bool" (con #f) ())) #t]
  [(falsy? (obj "tuple" (tuple ()) ())) #t]
  [(falsy? (obj "set" (set ()) ())) #t]
  [(falsy? (obj "dict" (dict ()) ())) #t]
  [(falsy? h) #f])
(module+ test
  (test-equal (term (do-if-exp (obj "int" (con 1) ()) "x" "y"))
              (term "x"))
  (test-equal (term (do-if-exp (obj "int" (con 0) ()) "x" "y"))
              (term "y")))
(define-metafunction SP-dynamics
  do-if-exp : h e- e- -> e-
  [(do-if-exp h e-_thn e-_els)
   e-_els
   (where #t (falsy? h))]
  [(do-if-exp h e-_thn e-_els)
   e-_thn])


(define-metafunction SP-dynamics
  do-assign-attribute : Σ mode l x v -> [Σ s-]
  [(do-assign-attribute Σ fast l_obj x_mem (ref l_new))
   (do-assign-attribute-fast Σ l_obj x_mem (ref l_new))]
  [(do-assign-attribute Σ safe l_obj x_mem (ref l_new))
   (do-assign-attribute-safe Σ l_obj x_mem (ref l_new))])
(define-metafunction SP-dynamics
  do-assign-attribute-fast : Σ l x v -> [Σ s-]
  [(do-assign-attribute-fast Σ l_obj x_mem (ref l_new))
   [(update Σ [l_obj (obj l_cls g (update ρ [x_mem l_new]))])
    (begin)]
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where (yes any) (lookup? ρ x_mem))]
  [(do-assign-attribute-fast Σ l_obj x_mem (ref l_new))
   [(update Σ [l_obj (obj l_cls g (extend ρ [x_mem l_new]))])
    (begin)]
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  do-assign-attribute-safe : Σ l x v -> [Σ s-]
  [(do-assign-attribute-safe Σ l_obj x_mem (ref l_new))
   (do-assign-attribute-fast Σ l_obj x_mem (ref l_new))
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where (yes any) (lookup? ρ x_mem))]
  [(do-assign-attribute-safe Σ l_obj x_mem (ref l_new))
   [Σ (raise (new "Exception" ((con ,(format "assign-attribute: undeclared field ~a" (term x_mem))))))]])
(define-metafunction SP-dynamics
  do-delete-attribute : Σ mode l x -> Σ
  [(do-delete-attribute Σ mode l_obj x_mem)
   (update Σ [l_obj (obj l_cls g (any_1 ... any_2 ...))])
   (where (obj l_cls g (any_1 ... [x_mem l_mem] any_2 ...)) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  do-if : h s- s- -> s-
  [(do-if h s-_thn s-_els)
   s-_els
   (where #t (falsy? h))]
  [(do-if h s-_thn s-_els)
   s-_thn])

(module+ test
  (test-->> red-p
            (term (load (local ("i") (assign "i" (con 42)))))
            (term (terminate)))
  (test-->> red-p
            (term (load (local () (expr (con 2)))))
            (term (terminate)))
  (test-->> red-p
            (term (load (local ("i") (begin (assign "i" (con 42)) (expr "i")))))
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]) 0 (expr (ref (con 2)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]) 0 (begin (begin (expr (con 2))) (expr (con 3)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]) 0 (if (ref (con 0))
                                          (expr (con 2))
                                          (expr (con 3)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env (["abc" (ref (con 2))]) ☠)]) 0 (delete "abc")])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]
                    [1 (obj "MyClass" (nothing) (["abc" (con 2)]))])
                   0
                   (delete (attribute fast (ref 1) "abc"))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env (["abc" (ref (con 2))]) ☠)])
                   0
                   (assign "abc" (ref (con 3)))])
            (term (terminate)))
  (test-->> red-p
            (term [([0 (env () ☠)]
                    [1 (obj "MyClass" (nothing) (["abc" (con 2)]))])
                   0
                   (assign (attribute fast (ref 1) "abc") (ref (con 3)))])
            (term (terminate))))
(define red-p
  (reduction-relation
   SP-dynamics
   #:domain p
   ;; base case
   [--> [Σ l (begin)]
        (terminate)
        "terminate"]
   [--> [Σ l (raise (ref l_exn))]
        (error (string Σ l))
        (where (obj l_cls (con string) ρ) (lookup-Σ Σ l_exn))
        "error"]
   ;; enter
   [--> (in-hole [Σ l_0 se] (enter l_1 s-))
        (in-hole [Σ l_1 se] (leave l_0 s-))
        "enter"]
   ;; leave-return
   [--> (in-hole [Σ l_0 se] (leave l_1 (return v)))
        (in-hole [Σ l_1 se] v)
        "leave-return"]
   ;; leave-raise
   [--> (in-hole [Σ l_0 se] (leave l_1 (raise v)))
        (in-hole [Σ l_1 se] (raise v))
        "leave-raise"]
   [--> (in-hole [Σ l se] (in-hole ex (raise v)))
        (in-hole [Σ l se] (raise v))
        "raise 1"]
   [--> (in-hole [Σ l ss] (in-hole sx (raise v)))
        (in-hole [Σ l ss] (raise v))
        "raise 2"]
   [--> (in-hole [Σ l ss] (begin leave-begin s- ...))
        (in-hole [Σ l ss] leave-begin)
        "begin-leave"]
   [--> (in-hole [Σ l ss] (begin (begin) s- ...))
        (in-hole [Σ l ss] (begin s- ...))
        "begin-go-on"]
   [--> (in-hole [Σ l ss] (loop leave-loop s-))
        (in-hole [Σ l ss] leave-loop)
        "loop-leave"]
   [--> (in-hole [Σ l ss] (loop (begin) s-))
        (in-hole [Σ l ss] s-)
        "loop-go-on"]
   [--> (in-hole [Σ l ss] (loop continue s-))
        (in-hole [Σ l ss] s-)
        "loop-continue"]
   [--> (in-hole [Σ l ss] (loop break s-))
        (in-hole [Σ l ss] (begin))
        "loop-break"]
   ;; reduce statements
   [--> (in-hole [Σ l ss] (expr v))
        (in-hole [Σ l ss] (begin))
        "expr"]
   [--> (in-hole [Σ l ss] (if (ref l_cnd) s-_thn s-_els))
        (in-hole [Σ l ss] (do-if (lookup-Σ Σ l_cnd) s-_thn s-_els))
        "if"]
   [--> (in-hole [Σ_1 l ss] (delete x))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (update-env Σ_1 l x ☠))
        "delete"]
   [--> (in-hole [Σ_1 l ss] (delete (attribute mode (ref l_obj) x)))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (do-delete-attribute Σ_1 mode l_obj x))
        "delete attribute"]
   [--> (in-hole [Σ_1 l ss] (assign x_var v_new))
        (in-hole [Σ_2 l ss] (begin))
        (where Σ_2 (update-env Σ_1 l x_var v_new))
        "assign"]
   [--> (in-hole [Σ_1 l ss] (assign (attribute mode (ref l_obj) x_mem) v_new))
        (in-hole [Σ_2 l ss] s-)
        (where [Σ_2 s-] (do-assign-attribute Σ_1 mode l_obj x_mem v_new))
        "assign attribute"]
   [--> (in-hole [Σ_1 l_env ss] (import-from x_mod x_var))
        (in-hole [Σ_2 l_env ss] (begin))
        (where Σ_2 (do-import Σ_1 l_env x_mod x_var))
        "import-from"]
   [--> (in-hole [Σ l_env ss] (begin (begin) s- ...))
        (in-hole [Σ l_env ss] (begin s- ...))
        "begin"]
   [--> (in-hole [Σ l_env ss] (try leave-try e-_exn x_exn s-_exn s-_els))
        (in-hole [Σ l_env ss] leave-try)
        "try-leave"]
   [--> (in-hole [Σ l_env ss] (try (begin) e-_exn x_exn s-_exn s-_els))
        (in-hole [Σ l_env ss] s-_els)
        "try-else"]
   [--> (in-hole [Σ l_env ss] (try (raise v) e-_exn x_exn s-_exn s-_els))
        (in-hole [Σ l_env ss] (if (call-function (ref "isinstance") (v e-_exn))
                                  (begin
                                    (assign x_exn v)
                                    s-_exn)
                                  (raise v)))
        "try-catch"]
   [--> (in-hole [Σ l_env ss] (finally leave-final s-))
        (in-hole [Σ l_env ss] (begin s- leave-final))
        "finally-leave"]
   [--> (in-hole [Σ l_env ss] (while e- s-_thn s-_els))
        (in-hole [Σ l_env ss] (if e-
                                  (loop s-_thn (while e- s-_thn s-_els))
                                  s-_els))
        "while"]
   [--> (in-hole [Σ l_env se] x)
        (in-hole [Σ l_env se] v)
        (where v (lookup-env Σ l_env x))
        "variable"]
   [--> (in-hole [Σ l_env se] (con c))
        (in-hole [Σ l_env se] (ref (con c)))
        "constant"]
   [--> (in-hole [Σ_1 l_env se] (tuple (v ...)))
        (in-hole [Σ_2 l_env se] (ref l))
        (where [Σ_2 l] (alloc Σ_1 (obj "tuple" (tuple (v ...)) ())))
        "tuple"]
   [--> (in-hole [Σ_1 l_env se] (list (v ...)))
        (in-hole [Σ_2 l_env se] (ref l))
        (where [Σ_2 l] (alloc Σ_1 (obj "list" (list (v ...)) ())))
        "list"]
   [--> (in-hole [Σ_1 l_env se] (set (v ...)))
        (in-hole [Σ_2 l_env se] (ref l))
        (where [Σ_2 l] (alloc Σ_1 (obj "set" (set (v ...)) ())))
        "set"]
   [--> (in-hole [Σ_1 l_env se] (dict ([v_key v_val] ...)))
        (in-hole [Σ_2 l_env se] (ref l))
        (where [Σ_2 l] (alloc Σ_1 (obj "dict" (dict ([v_key v_val] ...)) ())))
        "dict"]
   [--> (in-hole [Σ l_env se] (is v_1 v_2))
        (in-hole [Σ l_env se] (ref (con (do-is v_1 v_2))))
        "is"]
   [--> (in-hole [Σ l_env se] (if-exp (ref l) e-_thn e-_els))
        (in-hole [Σ l_env se] (do-if-exp (lookup-Σ Σ l) e-_thn e-_els))
        "if-exp"]
   [--> (in-hole [Σ_0 l_env se] (attribute mode (ref l) x))
        (in-hole [Σ_1 l_env se] e-)
        (where [Σ_1 e-] (do-attribute Σ_0 mode l x))
        "attribute"]
   [--> (in-hole [Σ_1 l_env se] (lambda (x ...) s- level-))
        (in-hole [Σ_2 l_env se] (ref l_fun))
        (where [Σ_2 l_fun] (alloc Σ_1 (obj "function" (lambda l_env (x ...) s- level-) ())))
        "lambda"]
   [--> (in-hole [Σ_1 l_env se] (class x_cls (v ...)
                                  ([x_cmem s-_cmem] ...)
                                  ([x_imem s-_imem] ...)))
        (in-hole [Σ_2 l_env se] (ref l_cls))
        (where l_cls (user-defined-class x_cls))
        (where Σ_2 (extend Σ_1 [l_cls (obj "type"
                                           (class (v ...)
                                             ([x_cmem s-_cmem] ...)
                                             ([x_imem s-_imem] ...))
                                           ([x_cmem ☠] ...))]))
        "class"]
   [--> (in-hole [Σ_0 l_env0 se] (invoke-function l (v ...)))
        (in-hole [Σ_1 l_env1 se] e-)
        (where [Σ_1 l_env1 e-] (do-invoke-function-fast Σ_0 l_env0 (lookup-Σ Σ_0 l) (v ...)))
        "invoke-function"]
   [--> (in-hole [Σ_0 l_env0 se] (invoke-method l x v_obj (v_arg ...)))
        (in-hole [Σ_1 l_env1 se] e-)
        (where [Σ_1 l_env1 e-] (do-invoke-method Σ_0 l_env0 l x v_obj (v_arg ...)))
        "invoke-method"]
   [--> (in-hole [Σ_0 l_env0 se] (call-function (ref l_fun) (v_arg ...)))
        (in-hole [Σ_1 l_env1 se] e-)
        (where [Σ_1 l_env1 e-] (do-call-function Σ_0 l_env0 (lookup-Σ Σ_0 l_fun) (v_arg ...)))
        "call-function"]
   [--> (in-hole [Σ_0 l_env se] (new l_cls (v_arg ...)))
        (in-hole [Σ_1 l_env se] e-)
        (where [Σ_1 e-] (do-new Σ_0 l_cls v_arg ...))
        "new"]))
(define-metafunction SP-dynamics
  do-new : Σ l v ... -> [Σ e-]
  [(do-new Σ_0 l_cls v_arg ...)
   [Σ_1 (enter -1
               (begin
                 (expr (call-function (attribute fast (ref l_obj) "__init__")
                                      (v_arg ...)))
                 (return (ref l_obj))))]
   (where (x_mem ...) (instance-mem*-of-class Σ_0 l_cls))
   (where [Σ_1 l_obj] (alloc Σ_0 (obj l_cls (nothing) ([x_mem ☠] ...))))])
(define-metafunction SP-dynamics
  instance-mem*-of-class : Σ l -> (x ...)
  [(instance-mem*-of-class Σ "object")
   ()]
  [(instance-mem*-of-class Σ l)
   (append (x_imem ...) (x_prn ...))
   (where (obj "type" (class ((ref l_prn)) ([x_cmem s-_cmem] ...) ([x_imem s-_imem] ...)) ρ)
          (lookup-Σ Σ l))
   (where (x_prn ...) (instance-mem*-of-class Σ l_prn))])
(define-metafunction SP-dynamics
  do-import : Σ l x x -> Σ
  [(do-import Σ_1 l_env x_mod x_var)
   Σ_2
   (where (Type (subof l)) (T-of-import (import-from x_mod x_var)))
   (where Σ_2 (update-env Σ_1 l_env x_var (ref l)))]
  [(do-import Σ_1 l_env x_mod x_var)
   Σ_2
   (where (generic l) (T-of-import (import-from x_mod x_var)))
   (where Σ_2 (update-env Σ_1 l_env x_var (ref l)))]
  [(do-import Σ_1 l_env x_mod x_var)
   Σ_1])

(define-metafunction SP-dynamics
  calc : program- -> p
  [(calc program-)
   p
   (where (p) ,(apply-reduction-relation* red-p (term (load program-))))])

(define-metafunction SP-dynamics
  trace-calc : program- -> any
  [(trace-calc program-)
   ,(traces red-p (term (load program-)))])

(define-syntax-rule (test-run expect e)
  (test-match SP-dynamics expect (term (calc (compile-program (desugar-program e))))))

(define-syntax-rule (trace-run e)
  (traces red-p (term (load (compile-program (desugar-program e))))))

(test-run
 (terminate)
 ((import-from "__static__" ("PyDict"))))
(test-run
 (terminate)
 ((expr (con 42))))
(test-run
 (terminate)
 ((expr (bin-op + (con 2) (con 3)))))
(test-run
 (terminate)
 ((function-def "f" (["x" dynamic])
                dynamic
                ((return "x")))
  (expr (call "f" ((con 2))))))