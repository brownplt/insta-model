#lang racket
(require redex)
(require redex-abbrevs)
(require "grammar.rkt")
(require "statics-basic-definitions.rkt")
(provide (all-defined-out))

(define-judgment-form SP-statics
  #:mode (show I)
  #:contract (show any)

  [(where #f ,(and (pretty-write  (term any)) #f))
   (where #f ,(and (newline) #f))
   ------------------
   (show any)])

(define-metafunction SP-statics
  member : any (any ...) -> boolean
  [(member any_0 (any_1 ... any_0 any_2 ...)) #t]
  [(member any_0 (any_1 ...)) #f])

(define-judgment-form SP-statics
  #:mode (lookupo I I O)
  #:contract (lookupo ((any any) ...) any any)
  [(where #f (member any_key2 (any_key1 ...)))
   ---------------------- "Found"
   (lookupo ((any_key1 any_val1) ...
             (any_key2 any_val2)
             (any_key3 any_val3) ... )
            any_key2
            any_val2)])

(define-metafunction SP-statics
  lookup : ((any any) ...) any -> any
  [(lookup any_env any_key)
   any_val
   (judgment-holds (lookupo any_env any_key any_val))]
  [(lookup any_env any_key)
   ,(begin
      (writeln (term any_env))
      (writeln (term any_key))
      (error 'lookup "can't find ~e in ~e" (term any_key) (term any_env)))])

(define-metafunction SP-statics
  len : (any ...) -> number
  [(len (any ...)) ,(length (term (any ...)))])

(define-metafunction SP-statics
  = : any any -> boolean
  [(= any any) #t]
  [(= any_0 any_1) #f])

(define-metafunction SP-statics
  ≠ : any any -> boolean
  [(≠ any_0 any_1) ,(not (term (= any_0 any_1)))])

(define-metafunction SP-statics
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
(define-metafunction SP-statics
  update : ([any any] ...) [any any] ... -> ([any any] ...)
  [(update any_map) any_map]
  [(update any_map any_kv1 any_kv2 any_kv3 ...)
   (update (update any_map any_kv1) any_kv2 any_kv3 ...)]
  [(update ([any_key any_old] any_rst ...) [any_key any_new])
   ([any_key any_new] any_rst ...)]
  [(update (any_1 any_2 ...) [any_key any_val])
   (extend (update (any_2 ...) [any_key any_val]) any_1)])