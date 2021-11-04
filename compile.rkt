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
  (s- (define/assign x t e)
      ;   (define/assign (attribute e string) t e)
      ;   (def x ([x t] ...) t d s)
      ;   (class x (t ...) m ...)
      ;   (if e s s)
      (begin s- ...)
      ;   (delete x)
      ;   (delete (attribute e string))
      ;   (return e)
      ;   (expr e)
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
      ;  (tuple-syntax e ...)
      ;  (set-syntax e ...)
      (dict-syntax (e- e-) ...)
      ;  (is e e)
      ;  (is-not e e)
      ;  (if e e e)
      ;  (attribute e string)
      ;  (e e ...)
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
   (begin (compile-s s) ...)])

(define-metafunction SP-compiled
  compile-e : e -> e-
  [(compile-e x) x]
  [(compile-e c) c]
  [(compile-e (dict-syntax ([e_key e_val] ...)))
   (dict-syntax ([(compile-e e_key) (compile-e e_val)] ...))])