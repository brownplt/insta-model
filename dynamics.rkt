#lang racket
(require redex)
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
  ;; runtime representation of programs
  (p [Σ l s-]
     (error any)
     (terminate))
  ;; in expression reduce expression
  (ee hole
      (tuple (v ... ee e- ...))
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
      (delete (attribute ee x))
      (assign x ee)
      (assign (attribute mode ee x) e-)
      (assign (attribute mode v x) ee)
      (try se e- x s- s-)
      (finally se s-)
      (raise ee))
  ;; in statement reduce statement
  (ss hole
      (expr es)
      (return es)
      (begin ss s- ...)
      (if es s- s-)
      (delete (attribute es x))
      (assign x es)
      (assign (attribute es x) e-)
      (assign (attribute v x) es)
      (try ss e- x s- s-)
      (finally ss s-)
      (raise es))
  ;; in statement bubble up exception
  (sx (expr hole)
      (return hole)
      (begin hole s- ...)
      (if hole s- s-)
      (delete (attribute hole x))
      (assign x hole)
      (assign (attribute hole x) e-)
      (assign (attribute v x) hole)
      (raise hole))
  ;; in expression bubble up exception
  (ex (tuple (v ... hole e- ...))
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
   "issubclass"
   (method "object" "__init__")
   (method "Exception" "__init__")
   (method "int" "__add__")
   (method "int" "__sub__")
   (method "int" "__mul__")
   (method "int" "__div__")
   (method "dict" "__init__")
   (method "dict" "__getitem__")
   (method "dict" "__setitem__")
   (method "dict" "__delitem__")
   (method (chkdict T T) "__init__")
   (method (chkdict T T) "__getitem__")
   (method (chkdict T T) "__setitem__")
   (method (chkdict T T) "__delitem__")
   (method (chkdict T T) "__eq__")
   (method (chkdict T T) "clear"))
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
  [(lookup-Σ-primitive "isinstance")
   (obj "function"
        (lambda -1 ("ins" "cls")
          (begin)
          (local ("ins" "cls")
            (return (invoke-function "issubclass"
                                     ((invoke-function "type" ("ins"))
                                      "cls")))))
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
  delta : Σ prim-op-l (v ...) -> [Σ e-]
  [(delta Σ (method "object" "__init__") (v))
   [Σ v]]
  [(delta Σ (method "Exception" "__init__") (v ...))
   (delta-exception-init Σ v ...)]
  [(delta Σ (method "int" "__add__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(+ (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__sub__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(- (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__mul__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(* (term number_1) (term number_2))))]]
  [(delta Σ (method "int" "__div__") ((ref (con number_1)) (ref (con number_2))))
   [Σ (ref (con ,(/ (term number_1) (term number_2))))]]
  [(delta Σ (method "dict" "__delitem__") (v ...))
   (delta-dict-delitem Σ v ...)]
  [(delta Σ (method "dict" "__getitem__") (v ...))
   (delta-dict-getitem Σ v ...)]
  [(delta Σ (method "dict" "__setitem__") (v ...))
   (delta-dict-setitem Σ v ...)]
  [(delta Σ (method (chkdict T_key T_val) "__init__") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-init Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "clear") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-clear Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ (method (chkdict T_key T_val) "__eq__") ((ref l_obj) (ref l_arg) ...))
   (delta-checkeddict-eq Σ T_key T_val l_obj l_arg ...)]
  [(delta Σ "issubclass" (v ...))
   (delta-issubclass Σ v ...)]
  ;; fall back
  [(delta Σ l (v ...))
   [Σ (raise (new "Exception" ((con ,(format "delta: ~a" (term l))))))]])
(define-metafunction SP-dynamics
  delta-exception-init : Σ v ... -> [Σ e-]
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
  [(delta-dict-getitem Σ v ...)
   [Σ (raise (new "Exception" ((con "getitem"))))]])
(define-metafunction SP-dynamics
  delta-dict-setitem : Σ v ... -> [Σ e-]
  [(delta-dict-setitem Σ_0 (ref l_map) v_key v_new)
   [Σ_1 (ref (con None))]
   (where (obj l_typ (dict (vv_1 ... [v_key v_old] vv_2 ...)) ρ) (lookup-Σ Σ_0 l_map))
   (where Σ_1 (update Σ_0 [l_map (obj l_typ (dict (vv_1 ... [v_key v_new] vv_2 ...)) ρ)]))]
  [(delta-dict-setitem Σ_0 (ref l_map) v_key v_val)
   [Σ_1 (ref (con None))]
   (where (obj l_typ (dict (vv ...)) ρ) (lookup-Σ Σ_0 l_map))
   (where Σ_1 (update Σ_0 [l_map (obj l_typ (dict ([v_key v_val] vv ...)) ρ)]))]
  [(delta-dict-setitem Σ v ...)
   [Σ (raise (new "Exception" ((con "setitem"))))]])
(define-metafunction SP-dynamics
  delta-dict-delitem : Σ v ... -> [Σ e-]
  [(delta-dict-delitem Σ_0 (ref l_map) v_key)
   [Σ_1 (ref (con None))]
   (where (obj l_typ (dict (vv_1 ... [v_key v_val] vv_2 ...)) ρ) (lookup-Σ Σ_0 l_map))
   (where Σ_1 (update Σ_0 [l_map (obj l_typ (dict (vv_1 ... vv_2 ...)) ρ)]))]
  [(delta-dict-delitem Σ v ...)
   [Σ (raise (new "Exception" ((con "delitem"))))]])
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
   (do-invoke-function Σ l (obj "function" g ρ) (v ...))
   (where * (arity g))]
  [(do-call-function-check-arity Σ l (obj "function" g ρ) (v ...))
   (do-invoke-function Σ l (obj "function" g ρ) (v ...))
   (where #t ,(= (term (arity g)) (length (term (v ...)))))]
  [(do-call-function-check-arity Σ l (obj "function" g ρ) (v ...))
   [Σ l (enter -1
               (raise (new "Exception" ((con ,(format "arity-mismatch ~a: ~a vs ~a"
                                                      (term g)
                                                      (term (arity g))
                                                      (length (term (v ...)))))))))]])
(define-metafunction SP-dynamics
  arity : g -> number+*
  [(arity (prim-op l))
   (arity-prim-op l)]
  [(arity (lambda l_cls (x_arg ...) s-_chk (local (x_var ...) s-_bdy)))
   ,(length (term (x_arg ...)))])
(define-metafunction SP-dynamics
  arity-prim-op : prim-op-l -> number+*
  [(arity-prim-op "isinstance") 2]
  [(arity-prim-op "issubclass") 2]
  [(arity-prim-op (method "object" "__init__")) 1]
  [(arity-prim-op (method "Exception" "__init__")) *]
  [(arity-prim-op (method "int" "__add__")) 2]
  [(arity-prim-op (method "int" "__sub__")) 2]
  [(arity-prim-op (method "int" "__mul__")) 2]
  [(arity-prim-op (method "int" "__div__")) 2]
  [(arity-prim-op (method "dict" "__init__")) 2]
  [(arity-prim-op (method "dict" "__getitem__")) 2]
  [(arity-prim-op (method "dict" "__setitem__")) 3]
  [(arity-prim-op (method "dict" "__delitem__")) 2]
  [(arity-prim-op (method (chkdict T_key T_val) "__init__")) *]
  [(arity-prim-op (method (chkdict T_key T_val) "__getitem__")) 2]
  [(arity-prim-op (method (chkdict T_key T_val) "__setitem__")) 3]
  [(arity-prim-op (method (chkdict T_key T_val) "__delitem__")) 2])

(define-metafunction SP-dynamics
  do-invoke-method : Σ l l x v (v ...) -> [Σ l e-]
  ;; This function assumes that `v` is an instance of (a subclass of) `l`,
  ;;   and arguments `(v ...)` are well-typed.
  ;; invoke the method `x` declared in class `l`.
  [(do-invoke-method Σ l_env l_cls x v_obj (v_arg ...))
   [Σ l_env (call-function (attribute fast v_obj x) (v_arg ...))]])

(define-metafunction SP-dynamics
  do-invoke-function : Σ l h (v ...) -> [Σ l e-]
  ;; invoke the function `h` under the environment `l`.
  ;; This function assumes that `h` is an instance of `function`
  ;;   and arguments `(v ...)` are well-typed.
  ;; primitive function
  [(do-invoke-function Σ l_env (obj "function" (prim-op l) ()) (v ...))
   [Σ_1 l_env e-]
   (where [Σ_1 e-] (delta Σ l (v ...)))]
  ;; derived function
  [(do-invoke-function Σ l_env
                       (obj "function"
                            (lambda l_cls (x_arg ...)
                              s-_chk
                              (local (x_var ...) s-_bdy))
                            ρ)
                       (v_arg ...))
   (do-invoke-function-function Σ l_env l_cls (x_arg ...) s-_chk (x_var ...) s-_bdy ρ (v_arg ...))]
  ;; the type function
  [(do-invoke-function Σ l_env h ((ref l_arg)))
   [Σ l_env (ref l_cls)]
   (where h (lookup-Σ Σ "type"))
   (where (obj l_cls g ρ) (lookup-Σ Σ l_arg))])
(define-metafunction SP-dynamics
  do-invoke-function-function : Σ l l (x ...) s- (x ...) s- ρ (v ...) -> [Σ l e-]
  [(do-invoke-function-function Σ_0 l_env l_cls (x_arg ...) s-_chk (x_var ...) s-_bdy ρ (v_arg ...))
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
  do-attribute : Σ mode l x -> [Σ v]
  ;; When the attribute is defined by the object
  ;;   return the corresponding attribute
  [(do-attribute Σ mode l_obj x)
   [Σ (ref l_att)]
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where (yes l_att) (lookup? ρ x))]
  ;; When the attribute is defined by the class
  ;;   find the corresponding attribute
  ;;   and wrap it as necessary
  [(do-attribute Σ mode l_obj x)
   (wrap-attribute Σ (lookup-class-attribute Σ mode l_cls x) l_obj)
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  lookup-class-attribute : Σ mode l x -> l
  [(lookup-class-attribute Σ mode l_cls x)
   l_att
   (where (obj "type" g ρ) (lookup-Σ Σ l_cls))
   (where (yes l_att) (lookup? ρ x))]
  [(lookup-class-attribute Σ mode l_cls x)
   (lookup-class-attribute Σ mode l_prn x)
   (where (obj "type" (class ((ref l_prn)) ([x_cmem s-_cmem] ...) ([x_imem s-_imem] ...)) ρ)
          (lookup-Σ Σ l_cls))])
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
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))])
(define-metafunction SP-dynamics
  do-assign-attribute-safe : Σ l x v -> [Σ s-]
  [(do-assign-attribute-safe Σ l_obj x_mem (ref l_new))
   (do-assign-attribute-fast Σ l_obj x_mem (ref l_new))
   (where (obj l_cls g ρ) (lookup-Σ Σ l_obj))
   (where (yes any) (lookup? ρ x_mem))]
  [(do-assign-attribute-safe Σ l_obj x_mem (ref l_new))
   [Σ (raise (new "Exception" ((con "assign-attribute: undeclared field"))))]])
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
        (error string)
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
   [--> (in-hole [Σ l ss] (begin (return v) s- ...))
        (in-hole [Σ l ss] (return v))
        "return"]
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
   [--> (in-hole [Σ l_env ss] (try (return v) e-_exn x_exn s-_exn s-_els))
        (in-hole [Σ l_env ss] (return v))
        "try-return"]
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
   [--> (in-hole [Σ l_env ss] (finally (return v) s-))
        (in-hole [Σ l_env ss] (begin s- (return v)))
        "finally-return"]
   [--> (in-hole [Σ l_env ss] (finally (raise v) s-))
        (in-hole [Σ l_env ss] (begin s- (raise v)))
        "finally-raise"]
   [--> (in-hole [Σ l_env ss] (finally (begin) s-))
        (in-hole [Σ l_env ss] s-)
        "finally-done"]
   [--> (in-hole [Σ l_env ss] (while e- s-_thn s-_els))
        (in-hole [Σ l_env ss] (if e-
                                  (begin s-_thn (while e- s-_thn s-_els))
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
        (in-hole [Σ_1 l_env se] v)
        (where [Σ_1 v] (do-attribute Σ_0 mode l x))
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
        (where [Σ_1 l_env1 e-] (do-invoke-function Σ_0 l_env0 (lookup-Σ Σ_0 l) (v ...)))
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