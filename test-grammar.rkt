#lang racket
(require "grammar.rkt")
(require redex/reduction-semantics)

;; conformance_suite/CheckedDict_delete_bad_key.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "other"))))))

;; conformance_suite/CheckedDict_delete_checks_keys.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (delete (subscript (call "asDyn" ("x")) (con 42))))))

;; conformance_suite/CheckedDict_delete_good_key.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "bar"))))))

;; conformance_suite/CheckedDict_delete_neg.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (delete (subscript "x" (con 2))))))

;; conformance_suite/CheckedDict_delete_pos.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (delete (subscript "x" (con "foo"))))))

;; conformance_suite/CheckedDict_delete_then_lookup.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "bar"))) (expr (subscript "x" (con "bar"))))))

;; conformance_suite/CheckedDict_from_bad_dict.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 2) (con "a")) ((con 3) (con 4))))))))))

;; conformance_suite/CheckedDict_from_good_dict.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 2) (con "a")) ((con 3) (con "b"))))))))))

;; conformance_suite/CheckedDict_from_nondict.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ((con 42)))))))

;; conformance_suite/CheckedDict_insert.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (assign (subscript "x" (con "new")) (con 4)))))

;; conformance_suite/CheckedDict_insert_then_lookup.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict ())))) (assign (subscript "x" (con "foo")) (con 42)) (expr (subscript "x" (con "foo"))))))

;; conformance_suite/CheckedDict_is_inhabitable.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))))))

;; conformance_suite/CheckedDict_key_can_be_Optional.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "x" (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "int"))) (call (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "int"))) ((dict (((con "foo") (con 2)) ((con None) (con 3))))))) (assert (compare (subscript "x" (con None)) ((is (con 3))))))))

;; conformance_suite/CheckedDict_lookup_checks_keys.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (expr (subscript (call "asDyn" ("x")) (con 42))))))

;; conformance_suite/CheckedDict_lookup_key_neg.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (expr (subscript "x" (con 2))))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (expr (subscript "x" (con "foo"))))))

;; conformance_suite/CheckedDict_lookup_val_neg.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (ann-assign "y" "str" (subscript "x" (con "foo"))))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (ann-assign "y" "int" (subscript "x" (con "foo"))))))

;; conformance_suite/CheckedDict_update.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (assign (subscript "x" (con "bar")) (con 4)))))

;; conformance_suite/CheckedDict_update_checks_keys.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign (subscript (call "asDyn" ("x")) (con 42)) (con "bar")))))

;; conformance_suite/CheckedDict_update_checks_values.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign (subscript (call "asDyn" ("x")) (con "foo")) (con "bar")))))

;; conformance_suite/CheckedDict_update_key_neg.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign (subscript "x" (con 2)) (con 3)))))

;; conformance_suite/CheckedDict_update_key_pos.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign (subscript "x" (con "bar")) (con 3)))))

;; conformance_suite/CheckedDict_update_then_lookup.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign (subscript "x" (con "foo")) (con 3)) (assert (compare (subscript "x" (con "foo")) ((is (con 3))))))))

;; conformance_suite/CheckedDict_update_val_neg.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign (subscript "x" (con "bar")) (con "2")))))

;; conformance_suite/CheckedDict_update_val_pos.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign (subscript "x" (con "bar")) (con 2)))))

;; conformance_suite/CheckedDict_val_can_be_Optional.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "int")))) (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "int")))) ((dict (((con "foo") (con 2)) ((con "bar") (con None))))))) (assert (compare (subscript "x" (con "bar")) ((is (con None))))))))

;; conformance_suite/None_is_inhabitable.py
(test-match SP program+ (term ((ann-assign "x" (con None) (con None)))))

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "other"))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "bar"))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "bar"))) (expr (subscript "x" (con "bar"))))))

;; conformance_suite/PyDict_insert.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (assign (subscript "x" (con "new")) (con "hello")))))

;; conformance_suite/PyDict_insert_then_lookup.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict ())) (assign (subscript "x" (con "foo")) (con 42)) (expr (subscript "x" (con "foo"))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (expr (subscript "x" (con "other"))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (expr (subscript "x" (con "bar"))))))

;; conformance_suite/PyDict_update.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (assign (subscript "x" (con "bar")) (con "hello")))))

;; conformance_suite/PyDict_update_then_lookup.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con "foo") (con 2))))) (assign (subscript "x" (con "foo")) (con 3)) (assert (compare (subscript "x" (con "foo")) ((is (con 3))))))))

;; conformance_suite/assign_declared_field_neg.py
(test-match SP program+ (term ((class "B" () ((ann-assign "x" "str"))) (class "C" ("B") (pass)) (function-def "f" (("c" "C")) dynamic ((assign (attribute "c" "x") (con 42)))))))

;; conformance_suite/assign_declared_field_pos.py
(test-match SP program+ (term ((class "B" () ((ann-assign "x" "int"))) (class "C" ("B") (pass)) (function-def "f" (("c" "C")) dynamic ((assign (attribute "c" "x") (con 42)))))))

;; conformance_suite/bool_is_a_subtype_of_int_neg.py
(test-match SP program+ (term ((ann-assign "x" "bool" (con 42)))))

;; conformance_suite/bool_is_a_subtype_of_int_pos.py
(test-match SP program+ (term ((ann-assign "x" "int" (con #t)))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP program+ (term ((ann-assign "x" "bool" (con #t)))))

;; conformance_suite/child_is_a_subtype_of_parent_neg.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" ("C") (pass)) (ann-assign "x" "D" (call "C" ())))))

;; conformance_suite/child_is_a_subtype_of_parent_pos.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" ("C") (pass)) (ann-assign "x" "C" (call "D" ())))))

;; conformance_suite/classes_are_not_first-class.py
(test-match SP program+ (term ((class "C" () (pass)) (function-def "checkExpect" (("cls" dynamic) ("obj" dynamic)) dynamic ((ann-assign "x" "cls" "obj") (return "x"))) (expr (call "checkExpect" ("C" (con 42)))))))

;; conformance_suite/delete_declared_field.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "str"))) (function-def "f" (("c" "C")) dynamic ((delete (attribute "c" "x")))))))

;; conformance_suite/delete_undeclared_field.py
(test-match SP program+ (term ((class "C" () (pass)) (function-def "f" (("c" "C")) dynamic ((delete (attribute "c" "x")))))))

;; conformance_suite/downcast_int_to_bool_neg.py
(test-match SP program+ (term ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "int" (con 2)) (ann-assign "y" "bool" (call "asDyn" ("x"))))))

;; conformance_suite/downcast_int_to_bool_pos.py
(test-match SP program+ (term ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "int" (con #t)) (ann-assign "y" "bool" (call "asDyn" ("x"))))))

;; conformance_suite/dynamic_as_CheckedDict.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "id" (("x" dynamic)) (subscript "CheckedDict" (tuple ("str" "int"))) ((return "x"))))))

;; conformance_suite/dynamic_as_callable.py
(test-match SP program+ (term ((function-def "f" (("x" dynamic)) dynamic ((return (call "x" ((call "x" ((con 2))) (call "x" ((con "foo")))))))))))

;; conformance_suite/dynamic_as_int.py
(test-match SP program+ (term ((function-def "id" (("x" dynamic)) "int" ((return "x"))))))

;; conformance_suite/dynamic_as_user-defined_class.py
(test-match SP program+ (term ((class "C" () (pass)) (function-def "id" (("x" dynamic)) "C" ((return "x"))))))

;; conformance_suite/empty_program.py
(test-match SP program+ (term ()))

;; conformance_suite/init_checks_arity.py
(test-match SP program+ (term ((class "Person" () ((function-def "__init__" (("self" dynamic) ("name" "str") ("age" "int")) dynamic (pass)))) (assign "p1" (call "Person" ((con "Alice") (con 21) (con #f)))))))

;; conformance_suite/init_checks_type.py
(test-match SP program+ (term ((class "Person" () ((function-def "__init__" (("self" dynamic) ("name" "str") ("age" "int")) dynamic (pass)))) (assign "p1" (call "Person" ((con "Alice") (con "21")))))))

;; conformance_suite/insert_new_field.py
(test-match SP program+ (term ((class "C" () (pass)) (function-def "f" (("c" "C")) dynamic ((assign (attribute "c" "x") (con 42)))))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP program+ (term ((ann-assign "x" "int" (con 42)))))

;; conformance_suite/lookup_declared_field_neg.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "str"))) (function-def "expectInt" (("i" "int")) dynamic (pass)) (function-def "f" (("c" "C")) dynamic ((return (call "expectInt" ((attribute "c" "x")))))))))

;; conformance_suite/lookup_declared_field_pos.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "int"))) (function-def "expectInt" (("i" "int")) dynamic (pass)) (function-def "f" (("c" "C")) dynamic ((return (call "expectInt" ((attribute "c" "x")))))))))

;; conformance_suite/lookup_parent_field_neg.py
(test-match SP program+ (term ((class "B" () ((ann-assign "x" "str"))) (class "C" ("B") (pass)) (function-def "expectInt" (("i" "int")) dynamic (pass)) (function-def "f" (("c" "C")) dynamic ((return (call "expectInt" ((attribute "c" "x")))))))))

;; conformance_suite/lookup_parent_field_pos.py
(test-match SP program+ (term ((class "B" () ((ann-assign "x" "int"))) (class "C" ("B") (pass)) (function-def "expectInt" (("i" "int")) dynamic (pass)) (function-def "f" (("c" "C")) dynamic ((return (call "expectInt" ((attribute "c" "x")))))))))

;; conformance_suite/lookup_undeclared_field.py
(test-match SP program+ (term ((class "C" () (pass)) (function-def "expectInt" (("i" "int")) dynamic (pass)) (function-def "f" (("c" "C")) dynamic ((return (call "expectInt" ((attribute "c" "x")))))))))

;; conformance_suite/methods_check_input_arity.py
(test-match SP program+ (term ((class "C" () ((function-def "m" (("self" dynamic) ("x" dynamic)) dynamic (pass)))) (function-def "f" () dynamic ((expr (call (attribute (call "C" ()) "m") ((con 1) (con 2)))))))))

;; conformance_suite/methods_check_input_types.py
(test-match SP program+ (term ((class "C" () ((function-def "m" (("self" dynamic) ("x" "int")) dynamic ((return (con None)))))) (expr (call (attribute (call "C" ()) "m") ((con "foo")))))))

;; conformance_suite/methods_check_output_types.py
(test-match SP program+ (term ((class "C" () ((function-def "m" (("self" dynamic)) "int" ((return (con "foo")))))))))

;; conformance_suite/methods_work.py
(test-match SP program+ (term ((class "C" () ((function-def "m" (("self" dynamic) ("x" "int")) "str" ((return (con "foo")))))) (ann-assign "s" "str" (call (attribute (call "C" ()) "m") ((con 42)))))))

;; conformance_suite/optional_is_inhabitable_1.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (ann-assign "x" (subscript "Optional" "int") (con 42)))))

;; conformance_suite/optional_is_inhabitable_2.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (ann-assign "x" (subscript "Optional" "int") (con None)))))

;; conformance_suite/override_instance_field_with_imprecise_type.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((ann-assign "x" "str"))) (class "D" ("C") ((ann-assign "x" "Any"))))))

;; conformance_suite/override_instance_field_with_incompatible_type.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "int"))) (class "D" ("C") ((ann-assign "x" "str"))))))

;; conformance_suite/override_instance_field_with_method.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "int"))) (class "D" ("C") ((function-def "x" (("self" dynamic)) dynamic (pass)))))))

;; conformance_suite/override_instance_field_with_precise_type.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((ann-assign "x" "Any"))) (class "D" ("C") ((ann-assign "x" "str"))))))

;; conformance_suite/override_instance_field_with_same_type.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "int"))) (class "D" ("C") ((ann-assign "x" "int"))))))

;; conformance_suite/override_instance_field_with_subtype.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "int"))) (class "D" ("C") ((ann-assign "x" "bool"))))))

;; conformance_suite/override_instance_field_with_suptype.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "bool"))) (class "D" ("C") ((ann-assign "x" "int"))))))

;; conformance_suite/override_instance_method_contravariant_inputs_neg.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" ("C") (pass)) (class "A" () ((function-def "m" (("self" dynamic) ("x" "C")) (con None) ((return (con None)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("x" "D")) (con None) ((return (con None)))))))))

;; conformance_suite/override_instance_method_contravariant_inputs_pos.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" ("C") (pass)) (class "A" () ((function-def "m" (("self" dynamic) ("x" "D")) (con None) ((return (con None)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("x" "C")) (con None) ((return (con None)))))))))

;; conformance_suite/override_instance_method_covariant_output_neg.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" ("C") (pass)) (class "A" () ((function-def "m" (("self" dynamic)) "D" ((return (call "C" ())))))) (class "B" ("A") ((function-def "m" (("self" dynamic)) "C" ((return (call "C" ())))))))))

;; conformance_suite/override_instance_method_covariant_output_pos.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" ("C") (pass)) (class "A" () ((function-def "m" (("self" dynamic)) "C" ((return (call "C" ())))))) (class "B" ("A") ((function-def "m" (("self" dynamic)) "D" ((return (call "D" ())))))))))

;; conformance_suite/override_instance_method_with_field.py
(test-match SP program+ (term ((class "C" () ((function-def "x" (("self" dynamic)) dynamic (pass)))) (class "D" ("C") ((ann-assign "x" "int"))))))

;; conformance_suite/partially_static_class_update_dynamic_field.py
(test-match SP program+ (term ((function-def "make_C1" () dynamic ((class "C" () ((ann-assign "x" "str"))) (return "C"))) (assign "C1" (call "make_C1" ())) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign "c" (call "C2" ())) (assign (attribute "c" "abc") (con "foo")) (assert (compare (attribute "c" "abc") ((== (con "foo"))))))))

;; conformance_suite/procedure_check_argument_type_dynamically.py
(test-match SP program+ (term ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" (("x" "int")) dynamic (pass)) (expr (call "f" ((call "asDyn" ((con "foo")))))))))

;; conformance_suite/procedure_check_argument_type_statically.py
(test-match SP program+ (term ((function-def "f" (("x" "int")) dynamic (pass)) (expr (call "f" ((con "foo")))))))

;; conformance_suite/procedure_check_arity_dynamically.py
(test-match SP program+ (term ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic (pass)) (expr (call (call "asDyn" ("f")) ((con 2)))))))

;; conformance_suite/procedure_check_arity_statically.py
(test-match SP program+ (term ((function-def "f" (("x" dynamic) ("y" dynamic)) dynamic (pass)) (expr (call "f" ((con 2)))))))

;; conformance_suite/procedure_check_return_type_dynamically.py
(test-match SP program+ (term ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" () "str" ((return (call "asDyn" ((con 2)))))) (expr (call "f" ())))))

;; conformance_suite/procedure_check_return_type_statically.py
(test-match SP program+ (term ((function-def "f" () "str" ((return (con 2)))))))

;; conformance_suite/procedure_works.py
(test-match SP program+ (term ((function-def "f" (("x" "int") ("y" dynamic)) dynamic ((return (bin-op - "y" "x")))) (assert (compare (call "f" ((con 2) (con 3))) ((is (con 1))))))))

;; conformance_suite/redeclare_var_flatten_if.py
(test-match SP program+ (term ((if (con #t) ((assign "x" (con 2))) ((ann-assign "x" "int" (con 3)))))))

;; conformance_suite/redeclare_var_with_class.py
(test-match SP program+ (term ((ann-assign "x" "int" (con 2)) (class "x" () (pass)))))

;; conformance_suite/redeclare_var_with_def.py
(test-match SP program+ (term ((ann-assign "x" "int" (con 2)) (function-def "x" () dynamic (pass)))))

;; conformance_suite/redeclare_var_with_var_dyn_to_type.py
(test-match SP program+ (term ((assign "x" (con 2)) (ann-assign "x" "int" (con 3)))))

;; conformance_suite/redeclare_var_with_var_same_type.py
(test-match SP program+ (term ((ann-assign "x" "int" (con 2)) (ann-assign "x" "int" (con 3)))))

;; conformance_suite/slots_are_defined.py
(test-match SP program+ (term ((class "C" () (pass)) (assert (compare (attribute "C" "__slots__") ((== (tuple ()))))))))

;; conformance_suite/slots_contain_object_fields_but_not_class_fields.py
(test-match SP program+ (term ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int") (con 2)) (ann-assign "y" "str"))) (assert (compare (attribute "C" "__slots__") ((== (tuple ((con "y"))))))))))

;; conformance_suite/slots_do_not_accumulate.py
(test-match SP program+ (term ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assert (compare (attribute "C2" "__slots__") ((== (tuple ((con "y"))))))))))

;; conformance_suite/slots_do_not_contain_methods.py
(test-match SP program+ (term ((class "C" () ((function-def "mth1" (("self" dynamic)) dynamic (pass)))) (assert (compare (attribute "C" "__slots__") ((== (tuple ()))))))))

;; conformance_suite/static_class_update_dynamic_field.py
(test-match SP program+ (term ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign "c" (call "C2" ())) (assign (attribute "c" "abc") (con "foo")))))

;; conformance_suite/static_class_update_static_field.py
(test-match SP program+ (term ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign "c" (call "C2" ())) (assign (attribute "c" "x") (con "foo")) (assert (compare (attribute "c" "x") ((== (con "foo"))))))))

;; conformance_suite/str_is_inhabitable.py
(test-match SP program+ (term ((ann-assign "x" "str" (con "hello")))))

;; conformance_suite/subclass_builtin.py
(test-match SP program+ (term ((import-from "__static__" ("cast")) (class "C" ("int") (pass)) (ann-assign "x" "C" (call "C" ((con 42)))) (ann-assign "y" "int" "x"))))

;; conformance_suite/test_assert_narrowing_debug.py
(test-match SP program+ (term ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "int" ((assert (call "isinstance" ("x" "int"))) (return (bin-op + "x" (con 1))))))))

;; conformance_suite/test_assert_narrowing_not_isinstance_optimized.py
(test-match SP program+ (term ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "str" ((assert (unary-op not (call "isinstance" ("x" "int")))) (return "x"))))))

;; conformance_suite/test_assert_narrowing_optimized.py
(test-match SP program+ (term ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "object" ((assert (call "isinstance" ("x" "int"))) (return "x"))))))

;; conformance_suite/test_assign_from_generic_optional.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () (pass)) (function-def "f" (("x" "Optional")) dynamic ((ann-assign "y" (subscript "Optional" "C") "x"))))))

;; conformance_suite/test_assign_generic_optional.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" () dynamic ((ann-assign "x" "Optional" (con 42)))))))

;; conformance_suite/test_assign_generic_optional_2.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" () dynamic ((ann-assign "x" "Optional" (bin-op + (con 42) (con 1))))))))

;; conformance_suite/test_assign_subtype_handling.py
(test-match SP program+ (term ((class "B" () (pass)) (class "D" ("B") (pass)) (function-def "f" () dynamic ((ann-assign "b" "B" (call "B" ())) (assign "b" (call "D" ())) (assign "b" (call "B" ())))))))

;; conformance_suite/test_assign_subtype_handling_fail.py
(test-match SP program+ (term ((class "B" () (pass)) (class "D" ("B") (pass)) (function-def "f" () dynamic ((ann-assign "d" "D" (call "D" ())) (assign "d" (call "B" ())))))))

;; conformance_suite/test_assign_test_var.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) "int" ((if (compare "x" ((is (con None)))) ((assign "x" (con 1))) ()) (return "x"))))))

;; conformance_suite/test_assign_type_propagation.py
(test-match SP program+ (term ((function-def "test" () "int" ((assign "x" (con 5)) (return "x"))))))

;; conformance_suite/test_attr_generic_optional.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("x" "Optional")) dynamic ((return (attribute "x" "foo")))))))

;; conformance_suite/test_aug_assign.py
(test-match SP program+ (term ((function-def "f" (("l" dynamic)) dynamic ((aug-assign (subscript "l" (con 0)) + (con 1)))))))

;; conformance_suite/test_augassign_inexact.py
(test-match SP program+ (term ((function-def "something" () dynamic ((return (con 3)))) (function-def "t" () dynamic ((ann-assign "a" "int" (call "something" ())) (assign "b" (con 0)) (aug-assign "b" + "a") (return "b"))))))

;; conformance_suite/test_bind_boolop_type.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "f" (("self" dynamic)) "bool" ((return (con #t)))) (function-def "g" (("self" dynamic)) "bool" ((return (con #f)))) (function-def "x" (("self" dynamic)) "bool" ((return (bool-op and ((call (attribute "self" "f") ()) (call (attribute "self" "g") ())))))) (function-def "y" (("self" dynamic)) "bool" ((return (bool-op or ((call (attribute "self" "f") ()) (call (attribute "self" "g") ())))))))))))

;; conformance_suite/test_bind_none_compare_op.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (function-def "has_none" (("x" dynamic)) "bool" ((return (compare (con None) ((in "x")))))) (function-def "has_no_none" (("x" dynamic)) "bool" ((return (compare (con None) ((not-in "x")))))))))

;; conformance_suite/test_bool_cast.py
(test-match SP program+ (term ((import-from "__static__" ("cast")) (class "D" () (pass)) (function-def "f" (("x" dynamic)) "bool" ((ann-assign "y" "bool" (call "cast" ("bool" "x"))) (return "y"))))))

;; conformance_suite/test_bool_int.py
(test-match SP program+ (term ((function-def "f" () dynamic ((ann-assign "x" "int" (con #t)) (return "x"))))))

;; conformance_suite/test_cast_unknown_type.py
(test-match SP program+ (term ((import-from "__static__" ("cast")) (function-def "f" () dynamic ((expr (call "cast" ("abc" (con 42)))))))))

;; conformance_suite/test_cast_wrong_args.py
(test-match SP program+ (term ((import-from "__static__" ("cast")) (function-def "f" () dynamic ((expr (call "cast" ((con 42)))))))))

;; conformance_suite/test_chkdict_literal.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (dict ())) (return "x"))))))

;; conformance_suite/test_class_static_tpflag.py
(test-match SP program+ (term ((class "A" () (pass)))))

;; conformance_suite/test_class_unknown_decorator.py
(test-match SP program+ (term ((function-def "dec" (("f" dynamic)) dynamic ((return "f"))) (class "C" () ((function-def "foo" (("self" dynamic)) "int" ((return (con 3)))) (function-def "f" (("self" dynamic)) dynamic ((return (call (attribute "self" "foo") ())))))))))

;; conformance_suite/test_compile_checked_dict_ann_differs.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("int" "int"))) (call (subscript "CheckedDict" (tuple ("str" "str"))) ((dict (((con "abc") (con "abc"))))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_ann_differs_2.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((ann-assign "x" "int" (call (subscript "CheckedDict" (tuple ("str" "str"))) ((dict (((con "abc") (con "abc"))))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict.py
(test-match SP program+ (term ((import-from "__static__" ("pydict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" "pydict" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict_as_dict.py
(test-match SP program+ (term ((import-from "__static__" ("pydict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" "dict" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_len.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (return (call "len" ("x"))))))))

;; conformance_suite/test_compile_checked_dict_opt_out_by_default.py
(test-match SP program+ (term ((class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign "x" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_optional.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("str" (bin-op bit-or "str" (con None))))) ((dict (((con "x") (con None)) ((con "y") (con "z"))))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_reversed.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "D" ()) (con 42)) ((call "B" ()) (con 42))))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_type_specified.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "D" ()) (con 42))))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_with_annotation.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (dict (((call "B" ()) (con 42))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_with_annotation_wrong_key_type.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (dict (((call "object" ()) (con 42))))) (return "x"))))))

;; conformance_suite/test_compile_checked_dict_with_annotation_wrong_value_type.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (dict (((call "B" ()) (con "hi"))))) (return "x"))))))

;; conformance_suite/test_compile_dict_get_typed.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 42) (con "abc"))))))) (ann-assign "y" (bin-op bit-or "str" (con None)) (call (attribute "x" "get") ((con 42)))))))))

;; conformance_suite/test_compile_dict_setdefault_typed.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 42) (con "abc"))))))) (ann-assign "y" (bin-op bit-or "str" (con None)) (call (attribute "x" "setdefault") ((con 100) (con "foo")))))))))

;; conformance_suite/test_compile_dict_setitem.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (expr (call (attribute "x" "__setitem__") ((con 2) (con "def")))) (return "x"))))))

;; conformance_suite/test_compile_dict_setitem_subscr.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (assign (subscript "x" (con 2)) (con "def")) (return "x"))))))

;; conformance_suite/test_compile_generic_dict_getitem_bad_type.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "abc") (con 42))))))) (return (subscript "x" (con 42))))))))

;; conformance_suite/test_compile_generic_dict_setitem_bad_type.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "abc") (con 42))))))) (assign (subscript "x" (con 42)) (con 42)))))))

;; conformance_suite/test_compile_generic_dict_setitem_bad_type_2.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "abc") (con 42))))))) (assign (subscript "x" (con "foo")) (con "abc")))))))

;; conformance_suite/test_compile_nested_class_in_fn.py
(test-match SP program+ (term ((function-def "fn" () dynamic ((class "C" () ((ann-assign "c" "int" (con 1)))) (return (call "C" ())))))))

;; conformance_suite/test_compile_nested_dict.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign "x" (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))))) (assign "y" (call (subscript "CheckedDict" (tuple ("int" (subscript "CheckedDict" (tuple ("B" "int")))))) ((dict (((con 42) "x")))))) (return "y"))))))

;; conformance_suite/test_decorated_function_ignored.py
(test-match SP program+ (term ((class "C" () (pass)) (function-def "mydecorator" (("x" dynamic)) dynamic ((return "C"))) (function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((return (call "f" ())))))))

;; conformance_suite/test_dict_invoke.py
(test-match SP program+ (term ((import-from "__static__" ("pydict")) (function-def "f" (("x" dynamic)) dynamic ((ann-assign "y" "pydict" "x") (return (call (attribute "y" "get") ((con "foo")))))))))

;; conformance_suite/test_dict_invoke_ret.py
(test-match SP program+ (term ((import-from "__static__" ("pydict")) (function-def "g" () dynamic ((return (con None)))) (function-def "f" (("x" dynamic)) dynamic ((ann-assign "y" "pydict" "x") (assign "z" (call (attribute "y" "get") ((con "foo")))) (assign "z" (con None)) (return "z"))))))

;; conformance_suite/test_duplicate_function_replaces_class.py
(test-match SP program+ (term ((class "X" () (pass)) (function-def "X" () dynamic (pass)))))

;; conformance_suite/test_duplicate_function_replaces_function.py
(test-match SP program+ (term ((function-def "f" () dynamic (pass)) (function-def "f" () dynamic (pass)))))

;; conformance_suite/test_final_constant_folding_disabled_on_nonfinals.py
(test-match SP program+ (term ((import-from "typing" ("Final")) (ann-assign "X" "str" (con "omg")) (function-def "f" () "str" ((return (subscript "X" (con 1))))))))

;; conformance_suite/test_final_in_args.py
(test-match SP program+ (term ((import-from "typing" ("Final")) (function-def "f" (("a" "Final")) (con None) (pass)))))

;; conformance_suite/test_generic_method_ret_type.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "MAP" (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) ((dict (((con "abc") (con "foo")) ((con "bar") (con None))))))) (function-def "f" (("x" "str")) (subscript "Optional" "str") ((return (call (attribute "MAP" "get") ("x"))))))))

;; conformance_suite/test_if_else_optional.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((assign (attribute "self" "field") "self"))))) (function-def "g" (("x" "C")) dynamic (pass)) (function-def "f" (("x" (subscript "Optional" "C")) ("y" (subscript "Optional" "C"))) dynamic ((if (compare "x" ((is (con None)))) ((assign "x" "y") (if (compare "x" ((is (con None)))) ((return (con None))) ((return (call "g" ("x")))))) ((return (call "g" ("x"))))) (return (con None)))))))

;; conformance_suite/test_if_else_optional_return.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((assign (attribute "self" "field") "self"))))) (function-def "f" (("x" (subscript "Optional" "C"))) dynamic ((if (compare "x" ((is (con None)))) ((return (con 0))) ()) (return (attribute "x" "field")))))))

;; conformance_suite/test_if_else_optional_return_in_else.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) "int" ((if (compare "x" ((is-not (con None)))) (pass) ((return (con 2)))) (return "x"))))))

;; conformance_suite/test_if_else_optional_return_in_else_assignment_in_if.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) "int" ((if (compare "x" ((is (con None)))) ((assign "x" (con 1))) ((return (con 2)))) (return "x"))))))

;; conformance_suite/test_if_else_optional_return_in_if_assignment_in_else.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) "int" ((if (compare "x" ((is-not (con None)))) ((return (con 2))) ((assign "x" (con 1)))) (return "x"))))))

;; conformance_suite/test_if_optional.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((assign (attribute "self" "field") (con 42)))))) (function-def "f" (("x" (subscript "Optional" "C"))) dynamic ((if (compare "x" ((is-not (con None)))) ((return (attribute "x" "field"))) ()) (return (con None)))))))

;; conformance_suite/test_if_optional_cond.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((assign (attribute "self" "field") (con 42)))))) (function-def "f" (("x" (subscript "Optional" "C"))) dynamic ((return (if-exp (compare "x" ((is-not (con None)))) (attribute "x" "field") (con None))))))))

;; conformance_suite/test_if_optional_dependent_conditions.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((ann-assign (attribute "self" "field") (subscript "Optional" "C") (con None)))))) (function-def "f" (("x" (subscript "Optional" "C"))) "C" ((if (bool-op and ((compare "x" ((is-not (con None)))) (compare (attribute "x" "field") ((is-not (con None)))))) ((return "x")) ()) (if (compare "x" ((is (con None)))) ((return (call "C" ()))) ()) (return "x"))))))

;; conformance_suite/test_incompat_override.py
(test-match SP program+ (term ((class "C" () ((ann-assign "x" "int"))) (class "D" ("C") ((function-def "x" (("self" dynamic)) dynamic (pass)))))))

;; conformance_suite/test_incompat_override_init_okay.py
(test-match SP program+ (term ((class "A" () ((function-def "__init__" (("self" dynamic)) (con None) (pass)))) (class "B" ("A") ((function-def "__init__" (("self" dynamic) ("x" "int")) (con None) (pass)))) (function-def "f" (("x" "A")) dynamic ((expr (call (attribute "x" "__init__") ())))))))

;; conformance_suite/test_incompat_override_method_arg_name.py
(test-match SP program+ (term ((class "A" () ((function-def "m" (("self" dynamic) ("x" "str")) "int" ((return (con 42)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("y" "str")) "int" ((return (con 0)))))))))

;; conformance_suite/test_incompat_override_method_arg_type.py
(test-match SP program+ (term ((class "A" () ((function-def "m" (("self" dynamic) ("x" "str")) "int" ((return (con 42)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("x" "int")) "int" ((return (con 0)))))))))

;; conformance_suite/test_incompat_override_method_arg_type_okay.py
(test-match SP program+ (term ((class "A" () ((function-def "m" (("self" dynamic) ("x" "str")) "int" ((return (con 42)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("x" "object")) "int" ((return (con 0)))))))))

;; conformance_suite/test_incompat_override_method_num_args.py
(test-match SP program+ (term ((class "A" () ((function-def "m" (("self" dynamic)) "int" ((return (con 42)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("x" "int")) "int" ((return (con 0)))))))))

;; conformance_suite/test_incompat_override_method_ret_type.py
(test-match SP program+ (term ((class "A" () ((function-def "m" (("self" dynamic)) "str" ((return (con "hello")))))) (class "B" ("A") ((function-def "m" (("self" dynamic)) "int" ((return (con 0)))))))))

;; conformance_suite/test_inline_nested.py
(test-match SP program+ (term ((import-from "__static__" ("inline")) (function-def "e" (("x" dynamic) ("y" dynamic)) dynamic ((return (bin-op + "x" "y")))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "e" ("x" (con 3)))))) (function-def "g" () dynamic ((return (call "f" ((con 1) (con 2)))))))))

;; conformance_suite/test_inline_nested_arg.py
(test-match SP program+ (term ((import-from "__static__" ("inline")) (function-def "e" (("x" dynamic) ("y" dynamic)) dynamic ((return (bin-op + "x" "y")))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "e" ("x" (con 3)))))) (function-def "g" (("a" dynamic) ("b" dynamic)) dynamic ((return (call "f" ("a" "b"))))))))

;; conformance_suite/test_inline_recursive.py
(test-match SP program+ (term ((import-from "__static__" ("inline")) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "f" ("x" "y"))))) (function-def "g" () dynamic ((return (call "f" ((con 1) (con 2)))))))))

;; conformance_suite/test_inline_return_type_mismatch.py
(test-match SP program+ (term ((import-from "__static__" ("inline")) (function-def "f" () "int" ((return (con 1)))) (function-def "g" () "str" ((return (call "f" ())))))))

;; conformance_suite/test_invoke_all_extra_args.py
(test-match SP program+ (term ((function-def "target" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic) ("g" dynamic)) dynamic ((return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * "a" (con 2)) (bin-op * "b" (con 3))) (bin-op * "c" (con 4))) (bin-op * "d" (con 5))) (bin-op * "e" (con 6))) (bin-op * "f" (con 7))) "g")))) (function-def "testfunc" () dynamic ((return (call "target" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6) (con 7)))))))))

;; conformance_suite/test_invoke_all_reg_args.py
(test-match SP program+ (term ((function-def "target" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic)) dynamic ((return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * "a" (con 2)) (bin-op * "b" (con 3))) (bin-op * "c" (con 4))) (bin-op * "d" (con 5))) (bin-op * "e" (con 6))) (bin-op * "f" (con 7)))))) (function-def "testfunc" () dynamic ((return (call "target" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6)))))))))

;; conformance_suite/test_invoke_chkdict_method.py
(test-match SP program+ (term ((import-from "__static__" ("CheckedDict")) (function-def "dict_maker" () (subscript "CheckedDict" (tuple ("int" "int"))) ((return (call (subscript "CheckedDict" (tuple ("int" "int"))) ((dict (((con 2) (con 2))))))))) (function-def "func" () dynamic ((assign "a" (call "dict_maker" ())) (return (call (attribute "a" "keys") ())))))))

;; conformance_suite/test_invoke_int_method.py
(test-match SP program+ (term ((function-def "func" () dynamic ((assign "a" (con 42)) (return (call (attribute "a" "bit_length") ())))))))

;; conformance_suite/test_invoke_method_non_static_base.py
(test-match SP program+ (term ((class "C" ("Exception") ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))) (function-def "g" (("self" dynamic)) dynamic ((return (call (attribute "self" "f") ())))))))))

;; conformance_suite/test_invoke_str_method_arg.py
(test-match SP program+ (term ((function-def "func" () dynamic ((assign "a" (con "a b c")) (return (call (attribute "a" "split") ((con "a")))))))))

;; conformance_suite/test_invoke_strict_module_deep.py
(test-match SP program+ (term ((function-def "f0" () dynamic ((return (con 42)))) (function-def "f1" () dynamic ((return (call "f0" ())))) (function-def "f2" () dynamic ((return (call "f1" ())))) (function-def "f3" () dynamic ((return (call "f2" ())))) (function-def "f4" () dynamic ((return (call "f3" ())))) (function-def "f5" () dynamic ((return (call "f4" ())))) (function-def "f6" () dynamic ((return (call "f5" ())))) (function-def "f7" () dynamic ((return (call "f6" ())))) (function-def "f8" () dynamic ((return (call "f7" ())))) (function-def "f9" () dynamic ((return (call "f8" ())))) (function-def "f10" () dynamic ((return (call "f9" ())))) (function-def "f11" () dynamic ((return (call "f10" ())))) (function-def "g" () dynamic ((return (call "f11" ())))))))

;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(test-match SP program+ (term ((function-def "f0" () dynamic ((return (con 42)))) (function-def "f1" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic) ("g" dynamic) ("h" dynamic)) dynamic ((class "C" () (pass)) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (call "f0" ()) "a") "b") "c") "d") "e") "f") "g") "h") (con 4))))) (function-def "f2" () dynamic ((return (call "f1" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6) (con 7) (con 8)))))) (function-def "f3" () dynamic ((return (call "f2" ())))) (function-def "f4" () dynamic ((return (call "f3" ())))) (function-def "f5" () dynamic ((return (call "f4" ())))) (function-def "f6" () dynamic ((return (call "f5" ())))) (function-def "f7" () dynamic ((return (call "f6" ())))) (function-def "f8" () dynamic ((return (call "f7" ())))) (function-def "f9" () dynamic ((return (call "f8" ())))) (function-def "f10" () dynamic ((return (call "f9" ())))) (function-def "f11" () dynamic ((return (call "f10" ())))) (function-def "g" () dynamic ((return (call "f11" ())))))))

;; conformance_suite/test_invoke_strict_module_pre_invoked.py
(test-match SP program+ (term ((function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((return (call "f" ())))))))

;; conformance_suite/test_max.py
(test-match SP program+ (term ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "max" ("a" "b"))))))))

;; conformance_suite/test_max_stability.py
(test-match SP program+ (term ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "max" ("a" "b"))))))))

;; conformance_suite/test_method_prologue_no_annotation.py
(test-match SP program+ (term ((function-def "f" (("x" dynamic)) dynamic ((return (con 42)))))))

;; conformance_suite/test_min.py
(test-match SP program+ (term ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "min" ("a" "b"))))))))

;; conformance_suite/test_min_stability.py
(test-match SP program+ (term ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "min" ("a" "b"))))))))

;; conformance_suite/test_module_level_final_decl.py
(test-match SP program+ (term ((import-from "typing" ("Final")) (ann-assign "x" "Final"))))

;; conformance_suite/test_module_subclass.py
(test-match SP program+ (term ((class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((ann-assign (attribute "self" "x") (subscript "Optional" "C") (con None)))))))))

;; conformance_suite/test_multiple_dynamic_base_class.py
(test-match SP program+ (term ((import-from "something" ("A" "B")) (class "C" ("A" "B") ((function-def "__init__" (("self" dynamic)) dynamic (pass)))))))

;; conformance_suite/test_narrow_or.py
(test-match SP program+ (term ((function-def "f" (("x" (bin-op bit-or "int" (con None)))) "int" ((if (bool-op or ((compare "x" ((is (con None)))) (compare "x" ((> (con 1)))))) ((assign "x" (con 1))) ()) (return "x"))))))

;; conformance_suite/test_no_narrow_to_dynamic.py
(test-match SP program+ (term ((function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((ann-assign "x" "int" (con 100)) (assign "x" (call "f" ())) (return (call (attribute "x" "bit_length") ())))))))

;; conformance_suite/test_none_annotation.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) (con None) ((return "x"))))))

;; conformance_suite/test_none_attribute_error.py
(test-match SP program+ (term ((function-def "f" () dynamic ((assign "x" (con None)) (return (attribute "x" "foo")))))))

;; conformance_suite/test_none_call.py
(test-match SP program+ (term ((function-def "f" () dynamic ((assign "x" (con None)) (return (call "x" ())))))))

;; conformance_suite/test_none_compare.py
(test-match SP program+ (term ((function-def "f" (("x" (bin-op bit-or "int" (con None)))) dynamic ((if (compare "x" ((> (con 1)))) ((assign "x" (con 1))) ()) (return "x"))))))

;; conformance_suite/test_none_compare_reverse.py
(test-match SP program+ (term ((function-def "f" (("x" (bin-op bit-or "int" (con None)))) dynamic ((if (compare (con 1) ((> "x"))) ((assign "x" (con 1))) ()) (return "x"))))))

;; conformance_suite/test_none_not.py
(test-match SP program+ (term ((function-def "t" () "bool" ((assign "x" (con None)) (if (unary-op not "x") ((return (con #t))) ((return (con #f)))))))))

;; conformance_suite/test_none_subscript.py
(test-match SP program+ (term ((function-def "f" () dynamic ((assign "x" (con None)) (return (subscript "x" (con 0))))))))

;; conformance_suite/test_optional_assign.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((function-def "f" (("self" dynamic) ("x" (subscript "Optional" (con "C")))) dynamic ((if (compare "x" ((is (con None)))) ((return "self")) ((ann-assign "p" (subscript "Optional" (con "C")) "x"))))))))))

;; conformance_suite/test_optional_assign_none.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "B" () (pass)) (function-def "f" (("x" (subscript "Optional" "B"))) dynamic ((ann-assign "a" (subscript "Optional" "B") (con None)))))))

;; conformance_suite/test_optional_assign_subclass.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "f" (("x" "D")) dynamic ((ann-assign "a" (subscript "Optional" "B") "x"))))))

;; conformance_suite/test_optional_assign_subclass_opt.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "f" (("x" (subscript "Optional" "D"))) dynamic ((ann-assign "a" (subscript "Optional" "B") "x"))))))

;; conformance_suite/test_optional_error.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (class "C" () ((ann-assign "x" (subscript "Optional" (con "C"))) (function-def "__init__" (("self" dynamic) ("set" dynamic)) dynamic ((if "set" ((assign (attribute "self" "x") "self")) ((assign (attribute "self" "x") (con None)))))) (function-def "f" (("self" dynamic)) (subscript "Optional" (con "C")) ((return (attribute (attribute "self" "x") "x")))))))))

;; conformance_suite/test_optional_subscript_error.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("a" (subscript "Optional" "int"))) dynamic ((expr (subscript "a" (con 1))))))))

;; conformance_suite/test_override_bad_ret.py
(test-match SP program+ (term ((class "B" () ((function-def "f" (("self" dynamic)) (con "B") ((return "self"))))) (function-def "f" (("x" "B")) dynamic ((return (call (attribute "x" "f") ())))))))

;; conformance_suite/test_override_okay.py
(test-match SP program+ (term ((class "B" () ((function-def "f" (("self" dynamic)) (con "B") ((return "self"))))) (function-def "f" (("x" "B")) dynamic ((return (call (attribute "x" "f") ())))))))

;; conformance_suite/test_package_no_parent.py
(test-match SP program+ (term ((class "C" () ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))))))))

;; conformance_suite/test_pydict_arg_annotation.py
(test-match SP program+ (term ((import-from "__static__" ("PyDict")) (function-def "f" (("d" (subscript "PyDict" (tuple ("str" "int"))))) "str" ((return (subscript "d" (con 3))))))))

;; conformance_suite/test_redefine_local_type.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" () (pass)) (function-def "f" () dynamic ((ann-assign "x" "C" (call "C" ())) (ann-assign "x" "D" (call "D" ())))))))

;; conformance_suite/test_redefine_type.py
(test-match SP program+ (term ((class "C" () (pass)) (class "D" () (pass)) (function-def "f" (("a" dynamic)) dynamic ((ann-assign "x" "C" (call "C" ())) (ann-assign "x" "D" (call "D" ())))))))

;; conformance_suite/test_refine_or_expression.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("s" (subscript "Optional" "str"))) "str" ((return (bool-op or ("s" (con "hi")))))))))

;; conformance_suite/test_refine_or_expression_with_multiple_optionals.py
(test-match SP program+ (term ((import-from "typing" ("Optional")) (function-def "f" (("s1" (subscript "Optional" "str")) ("s2" (subscript "Optional" "str"))) "str" ((return (bool-op or ("s1" "s2" (con "hi")))))))))

;; conformance_suite/test_ret_type_cast.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (function-def "testfunc" (("x" "str") ("y" "str")) "bool" ((return (compare "x" ((== "y")))))))))

;; conformance_suite/test_return_outside_func.py
(test-match SP program+ (term ((return (con 42)))))

;; conformance_suite/test_slotification_decorated.py
(test-match SP program+ (term ((class "_Inner" () (pass)) (function-def "something" (("klass" dynamic)) dynamic ((return "_Inner"))) (class "C" () ((function-def "f" (("self" dynamic)) dynamic (pass)))) (function-def "f" () dynamic ((return (call (attribute (call "C" ()) "f") ())))))))

;; conformance_suite/test_static_import_star.py
(test-match SP program+ (term ((import-from "__static__" ("*")))))

;; conformance_suite/test_static_import_unknown.py
(test-match SP program+ (term ((import-from "__static__" ("does_not_exist")))))

;; conformance_suite/test_str_split.py
(test-match SP program+ (term ((function-def "get_str" () "str" ((return (con "something here")))) (function-def "test" () "str" ((assign (tuple ("a" "b")) (call (attribute (call "get_str" ()) "split") ((con None) (con 1)))) (return "b"))))))

;; conformance_suite/test_type_of_or.py
(test-match SP program+ (term ((function-def "f" (("x" "int") ("y" "str")) (bin-op bit-or "int" "str") ((return (bool-op or ("x" "y"))))))))

;; conformance_suite/test_type_type_final.py
(test-match SP program+ (term ((class "A" ("type") (pass)))))

;; conformance_suite/test_unannotated_assign_no_later_declare.py
(test-match SP program+ (term ((function-def "f" (("flag" dynamic)) dynamic ((assign "x" (con None)) (if "flag" ((ann-assign "x" "str" (con "foo"))) ()))))))

;; conformance_suite/test_unknown_isinstance_bool_ret.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "__eq__" (("self" dynamic) ("other" "Any")) "bool" ((return (call "isinstance" ("other" "C"))))))))))

;; conformance_suite/test_unknown_isinstance_narrows.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))))) (function-def "testfunc" (("x" dynamic)) dynamic ((if (call "isinstance" ("x" "C")) ((return (attribute "x" "x"))) ()))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "f" (("self" dynamic) ("other" dynamic)) "str" ((if (call "isinstance" ("other" (attribute "self" "__class__"))) ((return (attribute "other" "x"))) ()) (return (con "")))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr_dynamic.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "f" (("self" dynamic) ("other" dynamic) ("unknown" dynamic)) dynamic ((if (call "isinstance" ("other" (attribute "unknown" "__class__"))) ((return (attribute "other" "x"))) ()) (return (con "")))))))))

;; conformance_suite/test_unknown_isinstance_narrows_else_correct.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))))) (function-def "testfunc" (("x" dynamic)) dynamic ((if (call "isinstance" ("x" "C")) (pass) ((return (attribute "x" "x")))))))))

;; conformance_suite/test_unknown_issubclass_bool_ret.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "__eq__" (("self" dynamic) ("other" "Any")) "bool" ((return (call "issubclass" ((call "type" ("other")) "C"))))))))))

;; conformance_suite/test_unknown_param_ann.py
(test-match SP program+ (term ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "__eq__" (("self" dynamic) ("other" "Any")) "bool" ((return (con #f)))))))))

;; conformance_suite/test_verify_lambda.py
(test-match SP program+ (term ((assign "x" (lambda (("x" dynamic)) "x")) (assign "a" (call "x" ((con "hi")))))))

;; conformance_suite/test_verify_positional_args_method.py
(test-match SP program+ (term ((class "C" () ((function-def "x" (("self" dynamic) ("a" "int") ("b" "str")) (con None) (pass)))) (expr (call (attribute (call "C" ()) "x") ((con 2) (con "hi")))))))

;; conformance_suite/test_verify_positional_args_unordered.py
(test-match SP program+ (term ((function-def "x" (("a" "int") ("b" "str")) (con None) ((return (call "y" ("a" "b"))))) (function-def "y" (("a" "int") ("b" "str")) (con None) (pass)))))

;; conformance_suite/test_visit_if_else.py
(test-match SP program+ (term ((assign "x" (con 0)) (if "x" (pass) ((function-def "f" () dynamic ((return (con 42)))))))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP program+ (term ((ann-assign "x" "bool" (con #t)) (ann-assign "y" "int" "x"))))