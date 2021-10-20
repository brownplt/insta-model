#lang racket
(require redex)
(require redex-abbrevs)
(require "model.rkt")
(require "statics.rkt")

;; conformance_suite/CheckedDict_delete_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x 2)))))

;; conformance_suite/CheckedDict_delete_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x "foo")))))

;; conformance_suite/CheckedDict_from_dict.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 "b")))))))

;; conformance_suite/CheckedDict_from_nondict.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) 42)))))

;; conformance_suite/CheckedDict_lookup_key_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (subscript x 2))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (subscript x "foo"))))

;; conformance_suite/CheckedDict_lookup_val_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y str (subscript x "foo")))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y int (subscript x "foo")))))

;; conformance_suite/CheckedDict_update_key_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x 2) 3))))

;; conformance_suite/CheckedDict_update_key_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") 3))))

;; conformance_suite/CheckedDict_update_val_neg.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") "2"))))

;; conformance_suite/CheckedDict_update_val_pos.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") 2))))

;; conformance_suite/None_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x None None))))

;; conformance_suite/PyDict_delete.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")))))

;; conformance_suite/PyDict_insert.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") "hello"))))

;; conformance_suite/PyDict_is_inhabitable.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (subscript x "bar"))))

;; conformance_suite/PyDict_lookup_good_key.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (subscript x "bar"))))

;; conformance_suite/PyDict_update.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") "hello"))))

;; conformance_suite/assign_declared_field_neg.py
(check-not-judgment-holds* (⊢p ((class B (object) (field "x" str)) (class C (B)) (def f ((c C)) dynamic (define/assign (attribute c "x") 42)))))

;; conformance_suite/assign_declared_field_pos.py
(check-judgment-holds* (⊢p ((class B (object) (field "x" int)) (class C (B)) (def f ((c C)) dynamic (define/assign (attribute c "x") 42)))))

;; conformance_suite/bool_is_a_subtype_of_int_neg.py
(check-not-judgment-holds* (⊢p ((define/assign x bool 42))))

;; conformance_suite/bool_is_a_subtype_of_int_pos.py
(check-judgment-holds* (⊢p ((define/assign x int #t))))

;; conformance_suite/bool_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x bool #t))))

;; conformance_suite/child_is_a_subtype_of_parent_neg.py
(check-not-judgment-holds* (⊢p ((class C (object)) (class D (C)) (define/assign x D (C)))))

;; conformance_suite/child_is_a_subtype_of_parent_pos.py
(check-judgment-holds* (⊢p ((class C (object)) (class D (C)) (define/assign x C (D)))))

;; conformance_suite/delete_declared_field.py
(check-judgment-holds* (⊢p ((class C (object) (field "x" str)) (def f ((c C)) dynamic (delete (attribute c "x"))))))

;; conformance_suite/delete_undeclared_field.py
(check-judgment-holds* (⊢p ((class C (object)) (def f ((c C)) dynamic (delete (attribute c "x"))))))

;; conformance_suite/dynamic_as_CheckedDict.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("CheckedDict")) (def id ((x dynamic)) (subscript CheckedDict (tuple-syntax str int)) (return x)))))

;; conformance_suite/dynamic_as_callable.py
(check-judgment-holds* (⊢p ((def f ((x dynamic)) dynamic (return (x (x 2) (x "foo")))))))

;; conformance_suite/dynamic_as_int.py
(check-judgment-holds* (⊢p ((def id ((x dynamic)) int (return x)))))

;; conformance_suite/dynamic_as_user-defined_class.py
(check-judgment-holds* (⊢p ((class C (object)) (def id ((x dynamic)) C (return x)))))

;; conformance_suite/init_checks_arity.py
(check-not-judgment-holds* (⊢p ((class Person (object) (method "__init__" self ((name str) (age int)) dynamic pass)) (define/assign p1 (Person "Alice" 21 #f)))))

;; conformance_suite/init_checks_type.py
(check-not-judgment-holds* (⊢p ((class Person (object) (method "__init__" self ((name str) (age int)) dynamic pass)) (define/assign p1 (Person "Alice" "21")))))

;; conformance_suite/insert_new_field.py
(check-judgment-holds* (⊢p ((class C (object)) (def f ((c C)) dynamic (define/assign (attribute c "x") 42)))))

;; conformance_suite/int_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x int 42))))

;; conformance_suite/lookup_declared_field_neg.py
(check-not-judgment-holds* (⊢p ((class C (object) (field "x" str)) (def expectInt ((i int)) dynamic pass) (def f ((c C)) dynamic (return (expectInt (attribute c "x")))))))

;; conformance_suite/lookup_declared_field_pos.py
(check-judgment-holds* (⊢p ((class C (object) (field "x" int)) (def expectInt ((i int)) dynamic pass) (def f ((c C)) dynamic (return (expectInt (attribute c "x")))))))

;; conformance_suite/lookup_parent_field_neg.py
(check-not-judgment-holds* (⊢p ((class B (object) (field "x" str)) (class C (B)) (def expectInt ((i int)) dynamic pass) (def f ((c C)) dynamic (return (expectInt (attribute c "x")))))))

;; conformance_suite/lookup_parent_field_pos.py
(check-judgment-holds* (⊢p ((class B (object) (field "x" int)) (class C (B)) (def expectInt ((i int)) dynamic pass) (def f ((c C)) dynamic (return (expectInt (attribute c "x")))))))

;; conformance_suite/lookup_undeclared_field.py
(check-judgment-holds* (⊢p ((class C (object)) (def expectInt ((i int)) dynamic pass) (def f ((c C)) dynamic (return (expectInt (attribute c "x")))))))

;; conformance_suite/methods_check_input_arity.py
(check-not-judgment-holds* (⊢p ((class C (object) (method "m" self ((x dynamic)) dynamic pass)) (def f () dynamic ((attribute (C) "m") 1 2)))))

;; conformance_suite/methods_check_input_types.py
(check-not-judgment-holds* (⊢p ((class C (object) (method "m" self ((x int)) dynamic (return None))) ((attribute (C) "m") "foo"))))

;; conformance_suite/methods_check_output_types.py
(check-not-judgment-holds* (⊢p ((class C (object) (method "m" self () int (return "foo"))))))

;; conformance_suite/methods_work.py
(check-judgment-holds* (⊢p ((class C (object) (method "m" self ((x int)) str (return "foo"))) (define/assign s str ((attribute (C) "m") 42)))))

;; conformance_suite/override_instance_field_with_imprecise_type.py
(check-not-judgment-holds* (⊢p ((import-from "typing" ("Any")) (class C (object) (field "x" str)) (class D (C) (field "x" Any)))))

;; conformance_suite/override_instance_field_with_incompatible_type.py
(check-not-judgment-holds* (⊢p ((class C (object) (field "x" int)) (class D (C) (field "x" str)))))

;; conformance_suite/override_instance_field_with_method.py
(check-not-judgment-holds* (⊢p ((class C (object) (field "x" int)) (class D (C) (method "x" self () dynamic pass)))))

;; conformance_suite/override_instance_field_with_precise_type.py
(check-not-judgment-holds* (⊢p ((import-from "typing" ("Any")) (class C (object) (field "x" Any)) (class D (C) (field "x" str)))))

;; conformance_suite/override_instance_field_with_same_type.py
(check-judgment-holds* (⊢p ((class C (object) (field "x" int)) (class D (C) (field "x" int)))))

;; conformance_suite/override_instance_field_with_subtype.py
(check-not-judgment-holds* (⊢p ((class C (object) (field "x" int)) (class D (C) (field "x" bool)))))

;; conformance_suite/override_instance_field_with_suptype.py
(check-not-judgment-holds* (⊢p ((class C (object) (field "x" bool)) (class D (C) (field "x" int)))))

;; conformance_suite/override_instance_method_contravariant_inputs_neg.py
(check-not-judgment-holds* (⊢p ((class C (object)) (class D (C)) (class A (object) (method "m" self ((x C)) None (return None))) (class B (A) (method "m" self ((x D)) None (return None))))))

;; conformance_suite/override_instance_method_contravariant_inputs_pos.py
(check-judgment-holds* (⊢p ((class C (object)) (class D (C)) (class A (object) (method "m" self ((x D)) None (return None))) (class B (A) (method "m" self ((x C)) None (return None))))))

;; conformance_suite/override_instance_method_covariant_output_neg.py
(check-not-judgment-holds* (⊢p ((class C (object)) (class D (C)) (class A (object) (method "m" self () D (return (C)))) (class B (A) (method "m" self () C (return (C)))))))

;; conformance_suite/override_instance_method_covariant_output_pos.py
(check-judgment-holds* (⊢p ((class C (object)) (class D (C)) (class A (object) (method "m" self () C (return (C)))) (class B (A) (method "m" self () D (return (D)))))))

;; conformance_suite/override_instance_method_with_field.py
(check-not-judgment-holds* (⊢p ((class C (object) (method "x" self () dynamic pass)) (class D (C) (field "x" int)))))

;; conformance_suite/str_is_inhabitable.py
(check-judgment-holds* (⊢p ((define/assign x str "hello"))))

;; conformance_suite/subclass_builtin.py
(check-judgment-holds* (⊢p ((import-from "__static__" ("cast")) (class C (int)) (define/assign x C (C 42)) (define/assign y int x))))

;; conformance_suite/test_bool_int.py
(check-judgment-holds* (⊢p ((def f () dynamic (define/assign x int #t) (return x)))))

;; conformance_suite/test_incompat_override.py
(check-not-judgment-holds* (⊢p ((class C (object) (field "x" int)) (class D (C) (method "x" self () dynamic pass)))))

;; conformance_suite/test_incompat_override_method_arg_type_okay.py
(check-judgment-holds* (⊢p ((class A (object) (method "m" self ((x str)) int (return 42))) (class B (A) (method "m" self ((x object)) int (return 0))))))

;; conformance_suite/test_multiple_dynamic_base_class.py
(check-judgment-holds* (⊢p ((import-from "something" ("A" "B")) (class C (A B) (method "__init__" self () dynamic pass)))))

;; conformance_suite/test_redefine_local_type.py
(check-not-judgment-holds* (⊢p ((class C (object)) (class D (object)) (def f () dynamic (define/assign x C (C)) (define/assign x D (D))))))

;; conformance_suite/test_redefine_type.py
(check-not-judgment-holds* (⊢p ((class C (object)) (class D (object)) (def f ((a dynamic)) dynamic (define/assign x C (C)) (define/assign x D (D))))))

;; conformance_suite/test_static_import_star.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("*")))))

;; conformance_suite/test_static_import_unknown.py
(check-not-judgment-holds* (⊢p ((import-from "__static__" ("does_not_exist")))))

;; conformance_suite/test_type_type_final.py
(check-judgment-holds* (⊢p ((class A (type)))))

;; conformance_suite/test_verify_positional_args_method.py
(check-judgment-holds* (⊢p ((class C (object) (method "x" self ((a int) (b str)) None pass)) ((attribute (C) "x") 2 "hi"))))

;; conformance_suite/test_verify_positional_args_unordered.py
(check-judgment-holds* (⊢p ((def x ((a int) (b str)) None (return (y a b))) (def y ((a int) (b str)) None pass))))