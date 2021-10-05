#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "type-check.rkt")

;; conformance_suite/CheckedDict_from_dict_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" (CheckedDict)) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) 42)))))

;; conformance_suite/CheckedDict_from_dict_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" (CheckedDict)) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 "b")))))))

;; conformance_suite/CheckedDict_lookup_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" (PyDict CheckedDict)) (define/assign x (subscript CheckedDict (tuple-syntax str str)) ((subscript CheckedDict (tuple-syntax str str)) (dict-syntax ("foo" "bar")))) (define/assign y int (subscript x "foo")))))

;; conformance_suite/CheckedDict_lookup_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict CheckedDict)) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign y int (subscript x "foo")))))

;; conformance_suite/None_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x None None))))

;; conformance_suite/PyDict_delete.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict)) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")))))

;; conformance_suite/PyDict_insert.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict)) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") "hello"))))

;; conformance_suite/PyDict_is_inhabitable.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict)) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict)) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (subscript x "baz"))))

;; conformance_suite/PyDict_lookup_good_key.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict)) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (subscript x "bar"))))

;; conformance_suite/PyDict_update.py
(check-judgment-holds* (⊢p ((import-from "__static__" (PyDict)) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") "hello"))))

;; conformance_suite/bool_is_a_subtype_of_int_neg.py
(check-not-judgment-holds* (⊢p ((define/assign x bool 42))))

;; conformance_suite/bool_is_a_subtype_of_int_pos.py
(check-judgment-holds* (⊢p ((define/assign x int #t))))

;; conformance_suite/bool_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x bool #t))))

;; conformance_suite/child_is_a_subtype_of_parent_neg.py
(check-not-judgment-holds* (⊢p ((class C object) (class D C) (define/assign x D (C)))))

;; conformance_suite/child_is_a_subtype_of_parent_pos.py
(check-judgment-holds* (⊢p ((class C object) (class D C) (define/assign x C (D)))))

;; conformance_suite/init_checks_arity.py
(check-not-judgment-holds* (⊢p ((class Person object (method "__init__" self ((name str) (age int)) dynamic pass)) (define/assign p1 (Person "Alice" 21 #f)))))

;; conformance_suite/init_checks_type.py
(check-not-judgment-holds* (⊢p ((class Person object (method "__init__" self ((name str) (age int)) dynamic pass)) (define/assign p1 (Person "Alice" "21")))))

;; conformance_suite/int_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x int 42))))

;; conformance_suite/methods_check_inputs.py
(check-not-judgment-holds* (⊢p ((class C object (method "m" self ((x int)) dynamic (return None))) ((attribute (C) "m") "foo"))))

;; conformance_suite/methods_check_outputs.py
(check-not-judgment-holds* (⊢p ((class C object (method "m" self () int (return "foo"))))))

;; conformance_suite/override_instance_field.py
(check-not-judgment-holds* (⊢p ((class C object (field "x" int)) (class D C (field "x" int)))))

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

;; conformance_suite/str_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x str "hello"))))
