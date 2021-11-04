#lang racket
(require redex)
(require "./statics.rkt")
(provide (all-defined-out))

(define-extended-language SP-compiled SP-statics

  ;; program
  (program- (import-type ... d- s-))

  ;; declaration
  (d- (x ...))

  ;; statements
  (s- (define/assign x e-)
      ;   (define/assign (attribute e string) t e)
      ;   (def x ([x t] ...) t d s)
      ;   (class x (t ...) m ...)
      ;   (if e s s)
      (begin s- ...)
      ;   (delete x)
      ;   (delete (attribute e string))
      ;   (return e)
      (expr e-)
      ;   (claim x t)
      ;   pass
      )

  ;   (m (field string t)
  ;      (method string x ([x t] ...) t d s))

  ;   ;; type expression
  ;   (t dynamic
  ;      None ;; nonterminal x doesn't cover this because we mentioned None in c
  ;      ((attribute x string) (tuple-syntax t ...))
  ;      ((attribute x string) t)
  ;      (or-syntax t t)
  ;      x)

  (e- x
      c
      (tuple-syntax e- ...)
      (set-syntax e- ...)
      (dict-syntax (e- e-) ...)
      (is e- e-)
      (is-not e- e-)
      (if e- e- e-)
      (dynamic-attribute e- string)
      (static-attribute e- string)
      (e- e- ...)
      ;  (reveal-type any ... e)
      ;  (bool-op ob e e)
      ))

(define-metafunction SP-compiled
  compile-program : program -> program-
  [(compile-program (import-type ... d s))
   (import-type ... (compile-d d) (compile-s s))])

(define-metafunction SP-compiled
  compile-d : d -> d-
  [(compile-d ([x D] ...)) (x ...)])

(define-metafunction SP-compiled
  compile-s : s -> s-
  [(compile-s (define/assign x t e))
   (define/assign x (compile-e e))]
  [(compile-s (begin s ...))
   (begin (compile-s s) ...)]
  [(compile-s (expr e))
   (expr (compile-e e))])

(define-metafunction SP-compiled
  compile-e : e -> e-
  [(compile-e x) x]
  [(compile-e c) c]
  [(compile-e (set-syntax e ...))
   (set-syntax (compile-e e) ...)]
  [(compile-e (tuple-syntax e ...))
   (tuple-syntax (compile-e e) ...)]
  [(compile-e (dict-syntax [e_key e_val] ...))
   (dict-syntax [(compile-e e_key) (compile-e e_val)] ...)]
  [(compile-e (is e_lft e_rht))
   (is (compile-e e_lft) (compile-e e_rht))]
  [(compile-e (is-not e_lft e_rht))
   (is-not (compile-e e_lft) (compile-e e_rht))]
  [(compile-e (if e_cnd e_thn e_els))
   (if (compile-e e_cnd) (compile-e e_thn) (compile-e e_els))]
  [(compile-e (attribute e string))
   ;; TODO optimization
   (dynamic-attribute (compile-e e) string)]
  [(compile-e (e_fun e_arg ...))
   ((compile-e e_fun) (compile-e e_arg) ...)])