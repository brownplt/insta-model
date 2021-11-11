#lang racket
(require "desugar.rkt")
(require redex)

;; conformance_suite/CheckedDict_delete_bad_key.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "other"))))))

;; conformance_suite/CheckedDict_delete_checks_keys.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (delete (subscript (asDyn x) 42))))))

;; conformance_suite/CheckedDict_delete_good_key.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "bar"))))))

;; conformance_suite/CheckedDict_delete_neg.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x 2))))))

;; conformance_suite/CheckedDict_delete_pos.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x "foo"))))))

;; conformance_suite/CheckedDict_delete_then_lookup.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "bar")) (expr (subscript x "bar"))))))

;; conformance_suite/CheckedDict_from_bad_dict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 4))))))))

;; conformance_suite/CheckedDict_from_good_dict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 "b"))))))))

;; conformance_suite/CheckedDict_from_nondict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) 42))))))

;; conformance_suite/CheckedDict_insert.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (define/assign (subscript x "new") dynamic 4)))))

;; conformance_suite/CheckedDict_insert_then_lookup.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax))) (define/assign (subscript x "foo") dynamic 42) (expr (subscript x "foo"))))))

;; conformance_suite/CheckedDict_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3))))))))

;; conformance_suite/CheckedDict_key_can_be_Optional.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign x (subscript CheckedDict (tuple-syntax (subscript Optional str) int)) ((subscript CheckedDict (tuple-syntax (subscript Optional str) int)) (dict-syntax ("foo" 2) (None 3)))) (assert (is (subscript x None) 3))))))

;; conformance_suite/CheckedDict_lookup_checks_keys.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (expr (subscript (asDyn x) 42))))))

;; conformance_suite/CheckedDict_lookup_key_neg.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (expr (subscript x 2))))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (expr (subscript x "foo"))))))

;; conformance_suite/CheckedDict_lookup_val_neg.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y str (subscript x "foo"))))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y int (subscript x "foo"))))))

;; conformance_suite/CheckedDict_update.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (define/assign (subscript x "bar") dynamic 4)))))

;; conformance_suite/CheckedDict_update_checks_keys.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign (subscript (asDyn x) 42) dynamic "bar")))))

;; conformance_suite/CheckedDict_update_checks_values.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign (subscript (asDyn x) "foo") dynamic "bar")))))

;; conformance_suite/CheckedDict_update_key_neg.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x 2) dynamic 3)))))

;; conformance_suite/CheckedDict_update_key_pos.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic 3)))))

;; conformance_suite/CheckedDict_update_then_lookup.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign (subscript x "foo") dynamic 3) (assert (is (subscript x "foo") 3))))))

;; conformance_suite/CheckedDict_update_val_neg.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic "2")))))

;; conformance_suite/CheckedDict_update_val_pos.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic 2)))))

;; conformance_suite/CheckedDict_val_can_be_Optional.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign x (subscript CheckedDict (tuple-syntax str (subscript Optional int))) ((subscript CheckedDict (tuple-syntax str (subscript Optional int))) (dict-syntax ("foo" 2) ("bar" None)))) (assert (is (subscript x "bar") None))))))

;; conformance_suite/None_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((define/assign x None None)))))

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "other"))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar"))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")) (expr (subscript x "bar"))))))

;; conformance_suite/PyDict_insert.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") dynamic "hello")))))

;; conformance_suite/PyDict_insert_then_lookup.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax)) (define/assign (subscript x "foo") dynamic 42) (expr (subscript x "foo"))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2)))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "other"))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar"))))))

;; conformance_suite/PyDict_update.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") dynamic "hello")))))

;; conformance_suite/PyDict_update_then_lookup.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax ("foo" 2))) (define/assign (subscript x "foo") dynamic 3) (assert (is (subscript x "foo") 3))))))

;; conformance_suite/assign_declared_field_neg.py
(test-match SP-core program (term (desugar-program ((class B (object) (field "x" str)) (class C (B)) (def f ((c C)) dynamic (begin (define/assign (attribute c "x") dynamic 42)))))))

;; conformance_suite/assign_declared_field_pos.py
(test-match SP-core program (term (desugar-program ((class B (object) (field "x" int)) (class C (B)) (def f ((c C)) dynamic (begin (define/assign (attribute c "x") dynamic 42)))))))

;; conformance_suite/bool_is_a_subtype_of_int_neg.py
(test-match SP-core program (term (desugar-program ((define/assign x bool 42)))))

;; conformance_suite/bool_is_a_subtype_of_int_pos.py
(test-match SP-core program (term (desugar-program ((define/assign x int #t)))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((define/assign x bool #t)))))

;; conformance_suite/child_is_a_subtype_of_parent_neg.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (C)) (define/assign x D (C))))))

;; conformance_suite/child_is_a_subtype_of_parent_pos.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (C)) (define/assign x C (D))))))

;; conformance_suite/classes_are_not_first-class.py
(test-match SP-core program (term (desugar-program ((class C (object)) (def checkExpect ((cls dynamic) (obj dynamic)) dynamic (begin (define/assign x cls obj) (return x))) (expr (checkExpect C 42))))))

;; conformance_suite/delete_declared_field.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" str)) (def f ((c C)) dynamic (begin (delete (attribute c "x"))))))))

;; conformance_suite/delete_undeclared_field.py
(test-match SP-core program (term (desugar-program ((class C (object)) (def f ((c C)) dynamic (begin (delete (attribute c "x"))))))))

;; conformance_suite/downcast_float_to_int_neg.py
(test-match SP-core program (term (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x float 2.3) (define/assign y int (asDyn x))))))

;; conformance_suite/downcast_float_to_int_pos.py
(test-match SP-core program (term (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x float 2) (define/assign y int (asDyn x))))))

;; conformance_suite/dynamic_as_CheckedDict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def id ((x dynamic)) (subscript CheckedDict (tuple-syntax str int)) (begin (return x)))))))

;; conformance_suite/dynamic_as_callable.py
(test-match SP-core program (term (desugar-program ((def f ((x dynamic)) dynamic (begin (return (x (x 2) (x "foo")))))))))

;; conformance_suite/dynamic_as_int.py
(test-match SP-core program (term (desugar-program ((def id ((x dynamic)) int (begin (return x)))))))

;; conformance_suite/dynamic_as_user-defined_class.py
(test-match SP-core program (term (desugar-program ((class C (object)) (def id ((x dynamic)) C (begin (return x)))))))

;; conformance_suite/empty_program.py
(test-match SP-core program (term (desugar-program ())))

;; conformance_suite/float_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((define/assign x float 2.3)))))

;; conformance_suite/init_checks_arity.py
(test-match SP-core program (term (desugar-program ((class Person (object) (method "__init__" self ((name str) (age int)) dynamic (begin pass))) (define/assign p1 dynamic (Person "Alice" 21 #f))))))

;; conformance_suite/init_checks_type.py
(test-match SP-core program (term (desugar-program ((class Person (object) (method "__init__" self ((name str) (age int)) dynamic (begin pass))) (define/assign p1 dynamic (Person "Alice" "21"))))))

;; conformance_suite/insert_new_field.py
(test-match SP-core program (term (desugar-program ((class C (object)) (def f ((c C)) dynamic (begin (define/assign (attribute c "x") dynamic 42)))))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((define/assign x int 42)))))

;; conformance_suite/lookup_declared_field_neg.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" str)) (def expectInt ((i int)) dynamic (begin pass)) (def f ((c C)) dynamic (begin (return (expectInt (attribute c "x")))))))))

;; conformance_suite/lookup_declared_field_pos.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" int)) (def expectInt ((i int)) dynamic (begin pass)) (def f ((c C)) dynamic (begin (return (expectInt (attribute c "x")))))))))

;; conformance_suite/lookup_parent_field_neg.py
(test-match SP-core program (term (desugar-program ((class B (object) (field "x" str)) (class C (B)) (def expectInt ((i int)) dynamic (begin pass)) (def f ((c C)) dynamic (begin (return (expectInt (attribute c "x")))))))))

;; conformance_suite/lookup_parent_field_pos.py
(test-match SP-core program (term (desugar-program ((class B (object) (field "x" int)) (class C (B)) (def expectInt ((i int)) dynamic (begin pass)) (def f ((c C)) dynamic (begin (return (expectInt (attribute c "x")))))))))

;; conformance_suite/lookup_undeclared_field.py
(test-match SP-core program (term (desugar-program ((class C (object)) (def expectInt ((i int)) dynamic (begin pass)) (def f ((c C)) dynamic (begin (return (expectInt (attribute c "x")))))))))

;; conformance_suite/methods_check_input_arity.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "m" self ((x dynamic)) dynamic (begin pass))) (def f () dynamic (begin (expr ((attribute (C) "m") 1 2))))))))

;; conformance_suite/methods_check_input_types.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "m" self ((x int)) dynamic (begin (return None)))) (expr ((attribute (C) "m") "foo"))))))

;; conformance_suite/methods_check_output_types.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "m" self () int (begin (return "foo"))))))))

;; conformance_suite/methods_work.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "m" self ((x int)) str (begin (return "foo")))) (define/assign s str ((attribute (C) "m") 42))))))

;; conformance_suite/optional_is_inhabitable_1.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (define/assign x (subscript Optional int) 42)))))

;; conformance_suite/optional_is_inhabitable_2.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (define/assign x (subscript Optional int) None)))))

;; conformance_suite/override_instance_field_with_imprecise_type.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (field "x" str)) (class D (C) (field "x" Any))))))

;; conformance_suite/override_instance_field_with_incompatible_type.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (field "x" str))))))

;; conformance_suite/override_instance_field_with_method.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (method "x" self () dynamic (begin pass)))))))

;; conformance_suite/override_instance_field_with_precise_type.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (field "x" Any)) (class D (C) (field "x" str))))))

;; conformance_suite/override_instance_field_with_same_type.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (field "x" int))))))

;; conformance_suite/override_instance_field_with_subtype.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (field "x" bool))))))

;; conformance_suite/override_instance_field_with_suptype.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" bool)) (class D (C) (field "x" int))))))

;; conformance_suite/override_instance_method_contravariant_inputs_neg.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self ((x C)) None (begin (return None)))) (class B (A) (method "m" self ((x D)) None (begin (return None))))))))

;; conformance_suite/override_instance_method_contravariant_inputs_pos.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self ((x D)) None (begin (return None)))) (class B (A) (method "m" self ((x C)) None (begin (return None))))))))

;; conformance_suite/override_instance_method_covariant_output_neg.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self () D (begin (return (C))))) (class B (A) (method "m" self () C (begin (return (C)))))))))

;; conformance_suite/override_instance_method_covariant_output_pos.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self () C (begin (return (C))))) (class B (A) (method "m" self () D (begin (return (D)))))))))

;; conformance_suite/override_instance_method_with_field.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "x" self () dynamic (begin pass))) (class D (C) (field "x" int))))))

;; conformance_suite/procedure_check_argument_type_dynamically.py
(test-match SP-core program (term (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (def f ((x int)) dynamic (begin pass)) (expr (f (asDyn "foo")))))))

;; conformance_suite/procedure_check_argument_type_statically.py
(test-match SP-core program (term (desugar-program ((def f ((x int)) dynamic (begin pass)) (expr (f "foo"))))))

;; conformance_suite/procedure_check_arity_dynamically.py
(test-match SP-core program (term (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (def f ((x dynamic) (y dynamic)) dynamic (begin pass)) (expr ((asDyn f) 2))))))

;; conformance_suite/procedure_check_arity_statically.py
(test-match SP-core program (term (desugar-program ((def f ((x dynamic) (y dynamic)) dynamic (begin pass)) (expr (f 2))))))

;; conformance_suite/procedure_check_return_type_dynamically.py
(test-match SP-core program (term (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (def f () str (begin (return (asDyn 2)))) (expr (f))))))

;; conformance_suite/procedure_check_return_type_statically.py
(test-match SP-core program (term (desugar-program ((def f () str (begin (return 2)))))))

;; conformance_suite/procedure_works.py
(test-match SP-core program (term (desugar-program ((def f ((x int) (y dynamic)) dynamic (begin (return (bin-op - y x)))) (assert (is (f 2 3) 1))))))

;; conformance_suite/redeclare_var_flatten_if.py
(test-match SP-core program (term (desugar-program ((if #t (begin (define/assign x dynamic 2)) (begin (define/assign x int 3)))))))

;; conformance_suite/redeclare_var_with_class.py
(test-match SP-core program (term (desugar-program ((define/assign x int 2) (class x (object))))))

;; conformance_suite/redeclare_var_with_def.py
(test-match SP-core program (term (desugar-program ((define/assign x int 2) (def x () dynamic (begin pass))))))

;; conformance_suite/redeclare_var_with_var_dyn_to_type.py
(test-match SP-core program (term (desugar-program ((define/assign x dynamic 2) (define/assign x int 3)))))

;; conformance_suite/redeclare_var_with_var_same_type.py
(test-match SP-core program (term (desugar-program ((define/assign x int 2) (define/assign x int 3)))))

;; conformance_suite/str_is_inhabitable.py
(test-match SP-core program (term (desugar-program ((define/assign x str "hello")))))

;; conformance_suite/subclass_builtin.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("cast")) (class C (int)) (define/assign x C (C 42)) (define/assign y int x)))))

;; conformance_suite/test_assert_narrowing_debug.py
(test-match SP-core program (term (desugar-program ((def foo ((x (or-syntax int str))) int (begin (assert (isinstance x int)) (return (bin-op + x 1))))))))

;; conformance_suite/test_assert_narrowing_not_isinstance_optimized.py
(test-match SP-core program (term (desugar-program ((def foo ((x (or-syntax int str))) str (begin (assert (unary-op not (isinstance x int))) (return x)))))))

;; conformance_suite/test_assert_narrowing_optimized.py
(test-match SP-core program (term (desugar-program ((def foo ((x (or-syntax int str))) object (begin (assert (isinstance x int)) (return x)))))))

;; conformance_suite/test_assign_from_generic_optional.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object)) (def f ((x Optional)) dynamic (begin (define/assign y (subscript Optional C) x)))))))

;; conformance_suite/test_assign_generic_optional.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f () dynamic (begin (define/assign x Optional 42)))))))

;; conformance_suite/test_assign_generic_optional_2.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f () dynamic (begin (define/assign x Optional (bin-op + 42 1))))))))

;; conformance_suite/test_assign_subtype_handling.py
(test-match SP-core program (term (desugar-program ((class B (object)) (class D (B)) (def f () dynamic (begin (define/assign b B (B)) (define/assign b dynamic (D)) (define/assign b dynamic (B))))))))

;; conformance_suite/test_assign_subtype_handling_fail.py
(test-match SP-core program (term (desugar-program ((class B (object)) (class D (B)) (def f () dynamic (begin (define/assign d D (D)) (define/assign d dynamic (B))))))))

;; conformance_suite/test_assign_test_var.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int (begin (if (is x None) (begin (define/assign x dynamic 1)) (begin)) (return x)))))))

;; conformance_suite/test_assign_type_propagation.py
(test-match SP-core program (term (desugar-program ((def test () int (begin (define/assign x dynamic 5) (return x)))))))

;; conformance_suite/test_attr_generic_optional.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x Optional)) dynamic (begin (return (attribute x "foo"))))))))

;; conformance_suite/test_aug_assign.py
(test-match SP-core program (term (desugar-program ((def f ((l dynamic)) dynamic (begin (define/assign (subscript l 0) dynamic (bin-op + (subscript l 0) 1))))))))

;; conformance_suite/test_augassign_inexact.py
(test-match SP-core program (term (desugar-program ((def something () dynamic (begin (return 3))) (def t () dynamic (begin (define/assign a int (something)) (define/assign b dynamic 0) (define/assign b dynamic (bin-op + b a)) (return b)))))))

;; conformance_suite/test_bind_boolop_type.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "f" self () bool (begin (return #t))) (method "g" self () bool (begin (return #f))) (method "x" self () bool (begin (return (bool-op and ((attribute self "f")) ((attribute self "g")))))) (method "y" self () bool (begin (return (bool-op or ((attribute self "f")) ((attribute self "g")))))))))))

;; conformance_suite/test_bind_none_compare_op.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (def has--none ((x dynamic)) bool (begin (return (in None x)))) (def has--no--none ((x dynamic)) bool (begin (return (not-in None x))))))))

;; conformance_suite/test_bool_cast.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("cast")) (class D (object)) (def f ((x dynamic)) bool (begin (define/assign y bool (cast bool x)) (return y)))))))

;; conformance_suite/test_bool_int.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin (define/assign x int #t) (return x)))))))

;; conformance_suite/test_call_function_unknown_ret_type.py
(test-match SP-core program (term (desugar-program ((import-from "__future__" ("annotations")) (def g () foo (begin (return 42))) (def testfunc () dynamic (begin (return (g))))))))

;; conformance_suite/test_cast_unknown_type.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("cast")) (def f () dynamic (begin (expr (cast abc 42))))))))

;; conformance_suite/test_cast_wrong_args.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("cast")) (def f () dynamic (begin (expr (cast 42))))))))

;; conformance_suite/test_chkdict_literal.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x (subscript CheckedDict (tuple-syntax int str)) (dict-syntax)) (return x)))))))

;; conformance_suite/test_class_static_tpflag.py
(test-match SP-core program (term (desugar-program ((class A (object))))))

;; conformance_suite/test_class_unknown_decorator.py
(test-match SP-core program (term (desugar-program ((def dec ((f dynamic)) dynamic (begin (return f))) (class C (object) (method "foo" self () int (begin (return 3))) (method "f" self () dynamic (begin (return ((attribute self "foo"))))))))))

;; conformance_suite/test_compile_checked_dict_ann_differs.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x (subscript CheckedDict (tuple-syntax int int)) ((subscript CheckedDict (tuple-syntax str str)) (dict-syntax ("abc" "abc")))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_ann_differs_2.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x int ((subscript CheckedDict (tuple-syntax str str)) (dict-syntax ("abc" "abc")))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("pydict")) (class B (object)) (class D (B)) (def testfunc () dynamic (begin (define/assign x pydict (dict-syntax ((B) 42) ((D) 42))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict_as_dict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("pydict")) (class B (object)) (class D (B)) (def testfunc () dynamic (begin (define/assign x dict (dict-syntax ((B) 42) ((D) 42))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_len.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (return (len x))))))))

;; conformance_suite/test_compile_checked_dict_opt_out_by_default.py
(test-match SP-core program (term (desugar-program ((class B (object)) (class D (B)) (def testfunc () dynamic (begin (define/assign x dynamic (dict-syntax ((B) 42) ((D) 42))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_optional.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax str (bin-op bit-or str None))) (dict-syntax ("x" None) ("y" "z")))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_reversed.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (class B (object)) (class D (B)) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((D) 42) ((B) 42)))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_type_specified.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (class B (object)) (class D (B)) (def testfunc () dynamic (begin (define/assign x (subscript CheckedDict (tuple-syntax B int)) ((subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((D) 42)))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_with_annotation.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (class B (object)) (def testfunc () dynamic (begin (define/assign x (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((B) 42))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_with_annotation_wrong_key_type.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (class B (object)) (def testfunc () dynamic (begin (define/assign x (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((object) 42))) (return x)))))))

;; conformance_suite/test_compile_checked_dict_with_annotation_wrong_value_type.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (class B (object)) (def testfunc () dynamic (begin (define/assign x (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((B) "hi"))) (return x)))))))

;; conformance_suite/test_compile_dict_get_typed.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (42 "abc")))) (define/assign y (or-syntax str None) ((attribute x "get") 42))))))))

;; conformance_suite/test_compile_dict_setdefault_typed.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (42 "abc")))) (define/assign y (or-syntax str None) ((attribute x "setdefault") 100 "foo"))))))))

;; conformance_suite/test_compile_dict_setitem.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (expr ((attribute x "__setitem__") 2 "def")) (return x)))))))

;; conformance_suite/test_compile_dict_setitem_subscr.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (define/assign (subscript x 2) dynamic "def") (return x)))))))

;; conformance_suite/test_compile_generic_dict_getitem_bad_type.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("abc" 42)))) (return (subscript x 42))))))))

;; conformance_suite/test_compile_generic_dict_setitem_bad_type.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("abc" 42)))) (define/assign (subscript x 42) dynamic 42)))))))

;; conformance_suite/test_compile_generic_dict_setitem_bad_type_2.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("abc" 42)))) (define/assign (subscript x "foo") dynamic "abc")))))))

;; conformance_suite/test_compile_nested_class_in_fn.py
(test-match SP-core program (term (desugar-program ((def fn () dynamic (begin (class C (object) (field "c" int 1)) (return (C))))))))

;; conformance_suite/test_compile_nested_dict.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (class B (object)) (class D (B)) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((B) 42) ((D) 42)))) (define/assign y dynamic ((subscript CheckedDict (tuple-syntax int (subscript CheckedDict (tuple-syntax B int)))) (dict-syntax (42 x)))) (return y)))))))

;; conformance_suite/test_decorated_function_ignored.py
(test-match SP-core program (term (desugar-program ((class C (object)) (def mydecorator ((x dynamic)) dynamic (begin (return C))) (def f () dynamic (begin (return 42))) (def g () dynamic (begin (return (f))))))))

;; conformance_suite/test_dict_invoke.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("pydict")) (def f ((x dynamic)) dynamic (begin (define/assign y pydict x) (return ((attribute y "get") "foo"))))))))

;; conformance_suite/test_dict_invoke_ret.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("pydict")) (def g () dynamic (begin (return None))) (def f ((x dynamic)) dynamic (begin (define/assign y pydict x) (define/assign z dynamic ((attribute y "get") "foo")) (define/assign z dynamic None) (return z)))))))

;; conformance_suite/test_duplicate_function_replaces_class.py
(test-match SP-core program (term (desugar-program ((class X (object)) (def X () dynamic (begin pass))))))

;; conformance_suite/test_duplicate_function_replaces_function.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin pass)) (def f () dynamic (begin pass))))))

;; conformance_suite/test_final_constant_folding_disabled_on_nonfinals.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Final")) (define/assign X str "omg") (def f () str (begin (return (subscript X 1))))))))

;; conformance_suite/test_final_in_args.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Final")) (def f ((a Final)) None (begin pass))))))

;; conformance_suite/test_generic_method_ret_type.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign MAP (subscript CheckedDict (tuple-syntax str (subscript Optional str))) ((subscript CheckedDict (tuple-syntax str (subscript Optional str))) (dict-syntax ("abc" "foo") ("bar" None)))) (def f ((x str)) (subscript Optional str) (begin (return ((attribute MAP "get") x))))))))

;; conformance_suite/test_if_else_optional.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (begin (define/assign (attribute self "field") dynamic self)))) (def g ((x C)) dynamic (begin pass)) (def f ((x (subscript Optional C)) (y (subscript Optional C))) dynamic (begin (if (is x None) (begin (define/assign x dynamic y) (if (is x None) (begin (return None)) (begin (return (g x))))) (begin (return (g x)))) (return None)))))))

;; conformance_suite/test_if_else_optional_return.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (begin (define/assign (attribute self "field") dynamic self)))) (def f ((x (subscript Optional C))) dynamic (begin (if (is x None) (begin (return 0)) (begin)) (return (attribute x "field"))))))))

;; conformance_suite/test_if_else_optional_return_in_else.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int (begin (if (is-not x None) (begin pass) (begin (return 2))) (return x)))))))

;; conformance_suite/test_if_else_optional_return_in_else_assignment_in_if.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int (begin (if (is x None) (begin (define/assign x dynamic 1)) (begin (return 2))) (return x)))))))

;; conformance_suite/test_if_else_optional_return_in_if_assignment_in_else.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int (begin (if (is-not x None) (begin (return 2)) (begin (define/assign x dynamic 1))) (return x)))))))

;; conformance_suite/test_if_optional.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (begin (define/assign (attribute self "field") dynamic 42)))) (def f ((x (subscript Optional C))) dynamic (begin (if (is-not x None) (begin (return (attribute x "field"))) (begin)) (return None)))))))

;; conformance_suite/test_if_optional_cond.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (begin (define/assign (attribute self "field") dynamic 42)))) (def f ((x (subscript Optional C))) dynamic (begin (return (if (is-not x None) (attribute x "field") None))))))))

;; conformance_suite/test_if_optional_dependent_conditions.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (begin (define/assign (attribute self "field") (subscript Optional C) None)))) (def f ((x (subscript Optional C))) C (begin (if (bool-op and (is-not x None) (is-not (attribute x "field") None)) (begin (return x)) (begin)) (if (is x None) (begin (return (C))) (begin)) (return x)))))))

;; conformance_suite/test_incompat_override.py
(test-match SP-core program (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (method "x" self () dynamic (begin pass)))))))

;; conformance_suite/test_incompat_override_init_okay.py
(test-match SP-core program (term (desugar-program ((class A (object) (method "__init__" self () None (begin pass))) (class B (A) (method "__init__" self ((x int)) None (begin pass))) (def f ((x A)) dynamic (begin (expr ((attribute x "__init__")))))))))

;; conformance_suite/test_incompat_override_method_arg_name.py
(test-match SP-core program (term (desugar-program ((class A (object) (method "m" self ((x str)) int (begin (return 42)))) (class B (A) (method "m" self ((y str)) int (begin (return 0))))))))

;; conformance_suite/test_incompat_override_method_arg_type.py
(test-match SP-core program (term (desugar-program ((class A (object) (method "m" self ((x str)) int (begin (return 42)))) (class B (A) (method "m" self ((x int)) int (begin (return 0))))))))

;; conformance_suite/test_incompat_override_method_arg_type_okay.py
(test-match SP-core program (term (desugar-program ((class A (object) (method "m" self ((x str)) int (begin (return 42)))) (class B (A) (method "m" self ((x object)) int (begin (return 0))))))))

;; conformance_suite/test_incompat_override_method_num_args.py
(test-match SP-core program (term (desugar-program ((class A (object) (method "m" self () int (begin (return 42)))) (class B (A) (method "m" self ((x int)) int (begin (return 0))))))))

;; conformance_suite/test_incompat_override_method_ret_type.py
(test-match SP-core program (term (desugar-program ((class A (object) (method "m" self () str (begin (return "hello")))) (class B (A) (method "m" self () int (begin (return 0))))))))

;; conformance_suite/test_inline_nested.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("inline")) (def e ((x dynamic) (y dynamic)) dynamic (begin (return (bin-op + x y)))) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (e x 3)))) (def g () dynamic (begin (return (f 1 2))))))))

;; conformance_suite/test_inline_nested_arg.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("inline")) (def e ((x dynamic) (y dynamic)) dynamic (begin (return (bin-op + x y)))) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (e x 3)))) (def g ((a dynamic) (b dynamic)) dynamic (begin (return (f a b))))))))

;; conformance_suite/test_inline_recursive.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("inline")) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (f x y)))) (def g () dynamic (begin (return (f 1 2))))))))

;; conformance_suite/test_inline_return_type_mismatch.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("inline")) (def f () int (begin (return 1))) (def g () str (begin (return (f))))))))

;; conformance_suite/test_invoke_all_extra_args.py
(test-match SP-core program (term (desugar-program ((def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7)) g)))) (def testfunc () dynamic (begin (return (target 1 2 3 4 5 6 7))))))))

;; conformance_suite/test_invoke_all_reg_args.py
(test-match SP-core program (term (desugar-program ((def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7))))) (def testfunc () dynamic (begin (return (target 1 2 3 4 5 6))))))))

;; conformance_suite/test_invoke_chkdict_method.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def dict--maker () (subscript CheckedDict (tuple-syntax int int)) (begin (return ((subscript CheckedDict (tuple-syntax int int)) (dict-syntax (2 2)))))) (def func () dynamic (begin (define/assign a dynamic (dict--maker)) (return ((attribute a "keys")))))))))

;; conformance_suite/test_invoke_int_method.py
(test-match SP-core program (term (desugar-program ((def func () dynamic (begin (define/assign a dynamic 42) (return ((attribute a "bit_length")))))))))

;; conformance_suite/test_invoke_method_non_static_base.py
(test-match SP-core program (term (desugar-program ((class C (Exception) (method "f" self () dynamic (begin (return 42))) (method "g" self () dynamic (begin (return ((attribute self "f"))))))))))

;; conformance_suite/test_invoke_str_method_arg.py
(test-match SP-core program (term (desugar-program ((def func () dynamic (begin (define/assign a dynamic "a b c") (return ((attribute a "split") "a"))))))))

;; conformance_suite/test_invoke_strict_module_deep.py
(test-match SP-core program (term (desugar-program ((def f0 () dynamic (begin (return 42))) (def f1 () dynamic (begin (return (f0)))) (def f2 () dynamic (begin (return (f1)))) (def f3 () dynamic (begin (return (f2)))) (def f4 () dynamic (begin (return (f3)))) (def f5 () dynamic (begin (return (f4)))) (def f6 () dynamic (begin (return (f5)))) (def f7 () dynamic (begin (return (f6)))) (def f8 () dynamic (begin (return (f7)))) (def f9 () dynamic (begin (return (f8)))) (def f10 () dynamic (begin (return (f9)))) (def f11 () dynamic (begin (return (f10)))) (def g () dynamic (begin (return (f11))))))))

;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(test-match SP-core program (term (desugar-program ((def f0 () dynamic (begin (return 42))) (def f1 ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic) (h dynamic)) dynamic (begin (class C (object)) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (f0) a) b) c) d) e) f) g) h) 4)))) (def f2 () dynamic (begin (return (f1 1 2 3 4 5 6 7 8)))) (def f3 () dynamic (begin (return (f2)))) (def f4 () dynamic (begin (return (f3)))) (def f5 () dynamic (begin (return (f4)))) (def f6 () dynamic (begin (return (f5)))) (def f7 () dynamic (begin (return (f6)))) (def f8 () dynamic (begin (return (f7)))) (def f9 () dynamic (begin (return (f8)))) (def f10 () dynamic (begin (return (f9)))) (def f11 () dynamic (begin (return (f10)))) (def g () dynamic (begin (return (f11))))))))

;; conformance_suite/test_invoke_strict_module_pre_invoked.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin (return 42))) (def g () dynamic (begin (return (f))))))))

;; conformance_suite/test_max.py
(test-match SP-core program (term (desugar-program ((def f ((a int) (b int)) int (begin (return (max a b))))))))

;; conformance_suite/test_max_stability.py
(test-match SP-core program (term (desugar-program ((def f ((a int) (b int)) int (begin (return (max a b))))))))

;; conformance_suite/test_method_prologue_no_annotation.py
(test-match SP-core program (term (desugar-program ((def f ((x dynamic)) dynamic (begin (return 42)))))))

;; conformance_suite/test_min.py
(test-match SP-core program (term (desugar-program ((def f ((a int) (b int)) int (begin (return (min a b))))))))

;; conformance_suite/test_min_stability.py
(test-match SP-core program (term (desugar-program ((def f ((a int) (b int)) int (begin (return (min a b))))))))

;; conformance_suite/test_module_level_final_decl.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Final")) (claim x Final)))))

;; conformance_suite/test_module_subclass.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "__init__" self () dynamic (begin (define/assign (attribute self "x") (subscript Optional C) None))))))))

;; conformance_suite/test_multiple_dynamic_base_class.py
(test-match SP-core program (term (desugar-program ((import-from "something" ("A" "B")) (class C (A B) (method "__init__" self () dynamic (begin pass)))))))

;; conformance_suite/test_named_tuple.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("NamedTuple")) (class C (NamedTuple) (field "x" int) (field "y" str)) (def myfunc ((x C)) dynamic (begin (return (attribute x "x"))))))))

;; conformance_suite/test_narrow_or.py
(test-match SP-core program (term (desugar-program ((def f ((x (or-syntax int None))) int (begin (if (bool-op or (is x None) (> x 1)) (begin (define/assign x dynamic 1)) (begin)) (return x)))))))

;; conformance_suite/test_no_narrow_to_dynamic.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin (return 42))) (def g () dynamic (begin (define/assign x int 100) (define/assign x dynamic (f)) (return ((attribute x "bit_length")))))))))

;; conformance_suite/test_none_annotation.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) None (begin (return x)))))))

;; conformance_suite/test_none_attribute_error.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin (define/assign x dynamic None) (return (attribute x "foo"))))))))

;; conformance_suite/test_none_call.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin (define/assign x dynamic None) (return (x))))))))

;; conformance_suite/test_none_compare.py
(test-match SP-core program (term (desugar-program ((def f ((x (or-syntax int None))) dynamic (begin (if (> x 1) (begin (define/assign x dynamic 1)) (begin)) (return x)))))))

;; conformance_suite/test_none_compare_reverse.py
(test-match SP-core program (term (desugar-program ((def f ((x (or-syntax int None))) dynamic (begin (if (> 1 x) (begin (define/assign x dynamic 1)) (begin)) (return x)))))))

;; conformance_suite/test_none_not.py
(test-match SP-core program (term (desugar-program ((def t () bool (begin (define/assign x dynamic None) (if (unary-op not x) (begin (return #t)) (begin (return #f)))))))))

;; conformance_suite/test_none_subscript.py
(test-match SP-core program (term (desugar-program ((def f () dynamic (begin (define/assign x dynamic None) (return (subscript x 0))))))))

;; conformance_suite/test_optional_assign.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "f" self ((x (subscript Optional "C"))) dynamic (begin (if (is x None) (begin (return self)) (begin (define/assign p (subscript Optional "C") x))))))))))

;; conformance_suite/test_optional_assign_none.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class B (object)) (def f ((x (subscript Optional B))) dynamic (begin (define/assign a (subscript Optional B) None)))))))

;; conformance_suite/test_optional_assign_subclass.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class B (object)) (class D (B)) (def f ((x D)) dynamic (begin (define/assign a (subscript Optional B) x)))))))

;; conformance_suite/test_optional_assign_subclass_opt.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class B (object)) (class D (B)) (def f ((x (subscript Optional D))) dynamic (begin (define/assign a (subscript Optional B) x)))))))

;; conformance_suite/test_optional_error.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (field "x" (subscript Optional "C")) (method "__init__" self ((set dynamic)) dynamic (begin (if set (begin (define/assign (attribute self "x") dynamic self)) (begin (define/assign (attribute self "x") dynamic None))))) (method "f" self () (subscript Optional "C") (begin (return (attribute (attribute self "x") "x")))))))))

;; conformance_suite/test_optional_subscript_error.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((a (subscript Optional int))) dynamic (begin (expr (subscript a 1))))))))

;; conformance_suite/test_override_bad_ret.py
(test-match SP-core program (term (desugar-program ((class B (object) (method "f" self () "B" (begin (return self)))) (def f ((x B)) dynamic (begin (return ((attribute x "f")))))))))

;; conformance_suite/test_override_okay.py
(test-match SP-core program (term (desugar-program ((class B (object) (method "f" self () "B" (begin (return self)))) (def f ((x B)) dynamic (begin (return ((attribute x "f")))))))))

;; conformance_suite/test_override_override_inherited.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (class B (object) (method "f" self () "Optional[B]" (begin (return self)))) (class D (B)) (def f ((x B)) dynamic (begin (return ((attribute x "f")))))))))

;; conformance_suite/test_package_no_parent.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "f" self () dynamic (begin (return 42))))))))

;; conformance_suite/test_prod_assert.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (import-from "__static__" ("prod_assert")) (def foo ((x (subscript Optional int))) int (begin (expr (prod--assert x)) (return x)))))))

;; conformance_suite/test_protocol_is_dynamic.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Protocol")) (class CallableProtocol (Protocol) (method "__call__" self ((x int)) str (begin pass))) (def foo ((x str)) int (begin (return (int x)))) (define/assign c CallableProtocol foo)))))

;; conformance_suite/test_pydict_arg_annotation.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("PyDict")) (def f ((d (subscript PyDict (tuple-syntax str int)))) str (begin (return (subscript d 3))))))))

;; conformance_suite/test_redefine_local_type.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (object)) (def f () dynamic (begin (define/assign x C (C)) (define/assign x D (D))))))))

;; conformance_suite/test_redefine_type.py
(test-match SP-core program (term (desugar-program ((class C (object)) (class D (object)) (def f ((a dynamic)) dynamic (begin (define/assign x C (C)) (define/assign x D (D))))))))

;; conformance_suite/test_refine_or_expression.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((s (subscript Optional str))) str (begin (return (bool-op or s "hi"))))))))

;; conformance_suite/test_refine_or_expression_with_multiple_optionals.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Optional")) (def f ((s1 (subscript Optional str)) (s2 (subscript Optional str))) str (begin (return (bool-op or s1 s2 "hi"))))))))

;; conformance_suite/test_ret_type_cast.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (def testfunc ((x str) (y str)) bool (begin (return (== x y))))))))

;; conformance_suite/test_return_outside_func.py
(test-match SP-core program (term (desugar-program ((return 42)))))

;; conformance_suite/test_slotification_decorated.py
(test-match SP-core program (term (desugar-program ((class --Inner (object)) (def something ((klass dynamic)) dynamic (begin (return --Inner))) (class C (object) (method "f" self () dynamic (begin pass))) (def f () dynamic (begin (return ((attribute (C) "f")))))))))

;; conformance_suite/test_static_import_star.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("*"))))))

;; conformance_suite/test_static_import_unknown.py
(test-match SP-core program (term (desugar-program ((import-from "__static__" ("does_not_exist"))))))

;; conformance_suite/test_str_split.py
(test-match SP-core program (term (desugar-program ((def get--str () str (begin (return "something here"))) (def test () str (begin (define/assign (tuple-syntax a b) dynamic ((attribute (get--str) "split") None 1)) (return b)))))))

;; conformance_suite/test_type_of_or.py
(test-match SP-core program (term (desugar-program ((def f ((x int) (y str)) (or-syntax int str) (begin (return (bool-op or x y))))))))

;; conformance_suite/test_type_type_final.py
(test-match SP-core program (term (desugar-program ((class A (type))))))

;; conformance_suite/test_unannotated_assign_no_later_declare.py
(test-match SP-core program (term (desugar-program ((def f ((flag dynamic)) dynamic (begin (define/assign x dynamic None) (if flag (begin (define/assign x str "foo")) (begin))))))))

;; conformance_suite/test_union_compare.py
(test-match SP-core program (term (desugar-program ((def f ((x (or-syntax int float))) bool (begin (return (> x 0))))))))

;; conformance_suite/test_unknown_isinstance_bool_ret.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "__eq__" self ((other Any)) bool (begin (return (isinstance other C)))))))))

;; conformance_suite/test_unknown_isinstance_narrows.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x)))) (def testfunc ((x dynamic)) dynamic (begin (if (isinstance x C) (begin (return (attribute x "x"))) (begin))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "f" self ((other dynamic)) str (begin (if (isinstance other (attribute self "__class__")) (begin (return (attribute other "x"))) (begin)) (return ""))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr_dynamic.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "f" self ((other dynamic) (unknown dynamic)) dynamic (begin (if (isinstance other (attribute unknown "__class__")) (begin (return (attribute other "x"))) (begin)) (return ""))))))))

;; conformance_suite/test_unknown_isinstance_narrows_else_correct.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x)))) (def testfunc ((x dynamic)) dynamic (begin (if (isinstance x C) (begin pass) (begin (return (attribute x "x"))))))))))

;; conformance_suite/test_unknown_issubclass_bool_ret.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "__eq__" self ((other Any)) bool (begin (return (issubclass (type other) C)))))))))

;; conformance_suite/test_unknown_param_ann.py
(test-match SP-core program (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (method "__init__" self ((x str)) dynamic (begin (define/assign (attribute self "x") str x))) (method "__eq__" self ((other Any)) bool (begin (return #f))))))))

;; conformance_suite/test_verify_lambda.py
(test-match SP-core program (term (desugar-program ((define/assign x dynamic (lambda ((x dynamic)) x)) (define/assign a dynamic (x "hi"))))))

;; conformance_suite/test_verify_positional_args_method.py
(test-match SP-core program (term (desugar-program ((class C (object) (method "x" self ((a int) (b str)) None (begin pass))) (expr ((attribute (C) "x") 2 "hi"))))))

;; conformance_suite/test_verify_positional_args_unordered.py
(test-match SP-core program (term (desugar-program ((def x ((a int) (b str)) None (begin (return (y a b)))) (def y ((a int) (b str)) None (begin pass))))))

;; conformance_suite/test_visit_if_else.py
(test-match SP-core program (term (desugar-program ((define/assign x dynamic 0) (if x (begin pass) (begin (def f () dynamic (begin (return 42)))))))))

;; conformance_suite/upcast_bool_to_float.py
(test-match SP-core program (term (desugar-program ((define/assign x bool #t) (define/assign y float x)))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP-core program (term (desugar-program ((define/assign x bool #t) (define/assign y int x)))))

;; conformance_suite/upcast_int_to_float.py
(test-match SP-core program (term (desugar-program ((define/assign x int 2) (define/assign y float x)))))
