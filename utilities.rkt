#lang racket
(require "grammar.rkt")
(require redex/reduction-semantics)
(provide (all-defined-out))


(define-judgment-form SP
  #:mode (show I)
  #:contract (show any)

  [(where #f ,(and (pretty-write  (term any)) #f))
   (where #f ,(and (newline) #f))
   ------------------
   (show any)])

(module+ test
  (test-equal (term (++ (x) (y z)))
              (term (x y z))))
(define-metafunction SP
  ++ : (any ...) (any ...) -> (any ...)
  [(++ (any_1 ...) (any_2 ...))
   (any_1 ... any_2 ...)])
(define-metafunction SP
  ++* : (any ...) ... -> (any ...)
  [(++*) ()]
  [(++* any_1 any_2 ...)
   (++ any_1 (++* any_2 ...))])

(define-judgment-form SP
  #:mode (member I I)
  #:contract (member any (any ...))
  [-----------
   (member any (any_1 ... any any_2 ...))])

(define-metafunction SP
  lookup : ((any any) ...) any -> any
  [(lookup ([any_key any_val] any_rst ...) any_key)
   any_val]
  [(lookup (any_1 any_2 ...) any_key)
   (lookup (any_2 ...) any_key)])

(define-metafunction SP
  lookup? : ((any any) ...) any -> any
  [(lookup? () any_key)
   (no ☠)]
  [(lookup? ([any_key any_val] any_rst ...) any_key)
   (yes any_val)]
  [(lookup? (any_1 any_2 ...) any_key)
   (lookup? (any_2 ...) any_key)])

(define-metafunction SP
  extend : ([any any] ...) [any any] ... -> ([any any] ...)
  [(extend (any_ent ...) [any_key any_val])
   ([any_key any_val] any_ent ...)]
  [(extend any_map) any_map]
  [(extend any_map any_kv1 any_kv2 ...)
   (extend (extend any_map any_kv1) any_kv2 ...)])

(module+ test
  (test-equal (term (update ([x 1] [y 2]) [x new-x]))
              (term ([x new-x] [y 2])))
  (test-equal (term (update ([x 1] [y 2]) [y new-y]))
              (term ([x 1] [y new-y])))
  (test-equal (term (update ([x 1] [y 2]) [x 3] [y 4]))
              (term ([x 3] [y 4])))
  (test-equal (term (update ([x 1] [y 2]) [y 4] [x 3]))
              (term ([x 3] [y 4]))))
(define-metafunction SP
  update : ([any any] ...) [any any] ... -> ([any any] ...)
  [(update any_map) any_map]
  [(update any_map any_kv1 any_kv2 any_kv3 ...)
   (update (update any_map any_kv1) any_kv2 any_kv3 ...)]
  [(update ([any_key any_old] any_rst ...) [any_key any_new])
   ([any_key any_new] any_rst ...)]
  [(update (any_1 any_2 ...) [any_key any_val])
   (extend (update (any_2 ...) [any_key any_val]) any_1)])