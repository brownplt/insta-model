#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "type-check.rkt")

;; conformance_suite/bool_is_a_subtype_of_int_neg.py
(check-not-judgment-holds* (⊢p ((define x bool 42))))

;; conformance_suite/bool_is_a_subtype_of_int_pos.py
(check-judgment-holds* (⊢p ((define x int #t))))

;; conformance_suite/child_is_a_subtype_of_parent_neg.py
(check-not-judgment-holds* (⊢p ((class C object) (class D C) (define x D (C)))))

;; conformance_suite/child_is_a_subtype_of_parent_pos.py
(check-judgment-holds* (⊢p ((class C object) (class D C) (define x C (D)))))

;; conformance_suite/init_checks_arity.py
(check-not-judgment-holds* (⊢p ((class Person object (method "__init__" self ((name str) (age int)) dynamic pass)) (define p1 (Person "Alice" 21 #f)))))

;; conformance_suite/init_checks_type.py
(check-not-judgment-holds* (⊢p ((class Person object (method "__init__" self ((name str) (age int)) dynamic pass)) (define p1 (Person "Alice" "21")))))

;; conformance_suite/override_instance_field_with_method.py
(check-not-judgment-holds* (⊢p ((class C object (field "x" int)) (class D C (method "x" self () dynamic pass)))))

;; conformance_suite/override_instance_method_contravariant_inputs_neg.py
(check-not-judgment-holds* (⊢p ((class C object) (class D C) (class A object (method "m" self ((x C)) None (return None))) (class B A (method "m" self ((x D)) None (return None))))))

;; conformance_suite/override_instance_method_contravariant_inputs_pos.py
(check-judgment-holds* (⊢p ((class C object) (class D C) (class A object (method "m" self ((x D)) None (return None))) (class B A (method "m" self ((x C)) None (return None))))))

;; conformance_suite/override_instance_method_covariant_output_neg.py
(check-not-judgment-holds* (⊢p ((class C object) (class D C) (class A object (method "m" self () D (return (C)))) (class B A (method "m" self () C (return (C)))))))

;; conformance_suite/override_instance_method_covariant_output_pos.py
(check-judgment-holds* (⊢p ((class C object) (class D C) (class A object (method "m" self () C (return (C)))) (class B A (method "m" self () D (return (D)))))))

;; conformance_suite/override_instance_method_with_field.py
(check-not-judgment-holds* (⊢p ((class C object (method "x" self () dynamic pass)) (class D C (field "x" int)))))
