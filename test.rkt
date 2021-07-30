#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "type-check.rkt")



(check-judgment-holds*
 (⊢p ())
 (⊢p (pass))
 (⊢p (pass
      pass))
 (⊢p (42))
 (⊢p (#t))
 (⊢p (#f))
 (⊢p ((define x int 42)))
 (⊢p ((define x int 42)
      x))
 (⊢p ((class C object)))
 (⊢p ((class C object
        (field x int)
        (method y self ((arg int)) int
                (return 42))))))


(check-not-judgment-holds*
 (⊢p ((class C object
        (field x int))
      (class D C
        (method x self () dynamic
                pass)))))


(check-not-judgment-holds*
 (⊢p ((class A object
        (method m self () str
                (return "hello")))
      (class B A
        (method m self () int
                (return 0))))))


(check-judgment-holds*
 (⊢p ((class C object
        (field x int))
      (class D C
        (field x int))))
 (⊢p ((class C object
        (field x int))
      (class D C
        (field x str)))))