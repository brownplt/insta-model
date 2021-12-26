#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")
(require "dynamics.rkt")

;; conformance_suite/CheckedDict_delete_bad_key.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "other")))))))))

;; conformance_suite/CheckedDict_delete_checks_keys.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (delete (subscript (call "asDyn" ("x")) (con 42)))))))))

;; conformance_suite/CheckedDict_delete_good_key.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "bar")))))))))

;; conformance_suite/CheckedDict_delete_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (delete (subscript "x" (con "foo")))))))))

;; conformance_suite/CheckedDict_delete_then_lookup.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "bar"))) (expr (subscript "x" (con "bar")))))))))

;; conformance_suite/CheckedDict_from_bad_dict.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("d") (dict (((con 2) (con "a")) ((con 3) (con 4))))) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ("d")))))))))

;; conformance_suite/CheckedDict_from_good_dict.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("d") (dict (((con 2) (con "a")) ((con 3) (con "b"))))) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ("d")))))))))

;; conformance_suite/CheckedDict_from_nondict.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ((con 42))))))))))

;; conformance_suite/CheckedDict_insert.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (assign ((subscript "x" (con "new"))) (con 4))))))))

;; conformance_suite/CheckedDict_insert_then_lookup.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict ())))) (assign ((subscript "x" (con "foo"))) (con 42)) (expr (subscript "x" (con "foo")))))))))

;; conformance_suite/CheckedDict_is_inhabitable.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3)))))))))))))

;; conformance_suite/CheckedDict_key_can_be_Optional.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "x" (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "int"))) (call (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "int"))) ((dict (((con "foo") (con 2)) ((con None) (con 3))))))) (assert (compare (subscript "x" (con None)) ((is (con 3)))))))))))

;; conformance_suite/CheckedDict_lookup_checks_keys.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (expr (subscript (call "asDyn" ("x")) (con 42)))))))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (expr (subscript "x" (con "foo")))))))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (ann-assign "y" "int" (subscript "x" (con "foo")))))))))

;; conformance_suite/CheckedDict_update.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (assign ((subscript "x" (con "bar"))) (con 4))))))))

;; conformance_suite/CheckedDict_update_checks_keys.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign ((subscript (call "asDyn" ("x")) (con 42))) (con "bar"))))))))

;; conformance_suite/CheckedDict_update_checks_values.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign ((subscript (call "asDyn" ("x")) (con "foo"))) (con "bar"))))))))

;; conformance_suite/CheckedDict_update_key_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign ((subscript "x" (con "bar"))) (con 3))))))))

;; conformance_suite/CheckedDict_update_then_lookup.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign ((subscript "x" (con "foo"))) (con 3)) (assert (compare (subscript "x" (con "foo")) ((is (con 3)))))))))))

;; conformance_suite/CheckedDict_update_val_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign ((subscript "x" (con "bar"))) (con 2))))))))

;; conformance_suite/CheckedDict_val_can_be_Optional.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "int")))) (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "int")))) ((dict (((con "foo") (con 2)) ((con "bar") (con None))))))) (assert (compare (subscript "x" (con "bar")) ((is (con None)))))))))))

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "other")))))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "bar")))))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "bar"))) (expr (subscript "x" (con "bar")))))))))

;; conformance_suite/PyDict_insert.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (assign ((subscript "x" (con "new"))) (con "hello"))))))))

;; conformance_suite/PyDict_insert_then_lookup.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict ())) (assign ((subscript "x" (con "foo"))) (con 42)) (expr (subscript "x" (con "foo")))))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2)))))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (expr (subscript "x" (con "other")))))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (expr (subscript "x" (con "bar")))))))))

;; conformance_suite/PyDict_update.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (assign ((subscript "x" (con "bar"))) (con "hello"))))))))

;; conformance_suite/PyDict_update_then_lookup.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con "foo") (con 2))))) (assign ((subscript "x" (con "foo"))) (con 3)) (assert (compare (subscript "x" (con "foo")) ((is (con 3)))))))))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "bool" (con #t))))))))

;; conformance_suite/class_variables_declare_and_init.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int") (con 42))))))))))

;; conformance_suite/class_variables_declare_only.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int"))))))))))

;; conformance_suite/class_variables_may_shadow.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C1" () ((ann-assign "x" (subscript "ClassVar" "int")))) (assign ((attribute "C1" "x")) (con 2)) (class "C2" ("C1") ((ann-assign "x" (subscript "ClassVar" "int")))) (assign ((attribute "C2" "x")) (con 3)) (assert (compare (attribute "C1" "x") ((is (con 2))))) (assert (compare (attribute "C2" "x") ((is (con 3)))))))))))

;; conformance_suite/class_variables_readable_at_instance_level.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int")))) (assign ((attribute "C" "x")) (con 42)) (assign ("obj") (call "C" ())) (assert (compare (attribute "obj" "x") ((is (con 42)))))))))))

;; conformance_suite/class_variables_redeclare_in_subclass_same_type.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C1" () ((ann-assign "x" (subscript "ClassVar" "int")))) (class "C2" ("C1") ((ann-assign "x" (subscript "ClassVar" "int"))))))))))

;; conformance_suite/class_variables_should_be_declared_with_ClassVar_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int") (con 42)))) (assert (compare (attribute "C" "x") ((is (con 42)))))))))))

;; conformance_suite/classes_are_not_first-class.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (function-def "checkExpect" (("cls" dynamic) ("obj" dynamic)) dynamic ((ann-assign "x" "cls" "obj") (return "x"))) (expr (call "checkExpect" ("C" (con 42))))))))))

;; conformance_suite/classes_work.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (assign ("o") (call "C" ()))))))))

;; conformance_suite/defs_and_lambdas_are_of_the_function_class.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((return (con 2)))) (assert (compare (call "type" ("f")) ((is (call "type" ((lambda () (con 3))))))))))))))

;; conformance_suite/downcast_int_to_bool_neg.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "int" (con 2)) (ann-assign "y" "bool" (call "asDyn" ("x")))))))))

;; conformance_suite/downcast_int_to_bool_pos.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "int" (con #t)) (ann-assign "y" "bool" (call "asDyn" ("x")))))))))

;; conformance_suite/empty_program.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ())))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "int" (con 42))))))))

;; conformance_suite/method_from_def.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((function-def "m" (("self" dynamic)) dynamic ((return (con 2)))))) (assign ("obj") (call "C" ())) (assert (compare (call (attribute "obj" "m") ()) ((is (con 2)))))))))))

;; conformance_suite/method_from_lambda.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any" "ClassVar")) (class "C" () ((ann-assign "m" (subscript "ClassVar" "Any") (lambda (("self" dynamic)) (con 2))))) (assign ("obj") (call "C" ())) (assert (compare (call (attribute "obj" "m") ()) ((is (con 2)))))))))))

;; conformance_suite/method_generative.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any" "ClassVar")) (class "C" () ((ann-assign "m1" (subscript "ClassVar" "Any") (lambda (("self" dynamic)) (con 2))) (function-def "m2" (("self" dynamic)) dynamic ((return (con 3)))))) (assign ("obj") (call "C" ())) (assert (compare (attribute "obj" "m1") ((is-not (attribute "obj" "m1"))))) (assert (compare (attribute "obj" "m2") ((is-not (attribute "obj" "m2")))))))))))

;; conformance_suite/methods_can_be_declared_as_class_variables.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar" "Any")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "Any") (lambda (("self" dynamic) ("n" dynamic)) (bin-op + "n" (con 1)))))) (assign ("o") (call "C" ())) (assert (compare (call (attribute "o" "x") ((con 2))) ((is (con 3)))))))))))

;; conformance_suite/procedure_check_argument_type_dynamically.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" (("x" "int")) dynamic (pass)) (expr (call "f" ((call "asDyn" ((con "foo"))))))))))))

;; conformance_suite/procedure_check_arity_dynamically.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic (pass)) (expr (call (call "asDyn" ("f")) ((con 2))))))))))

;; conformance_suite/procedure_check_return_type_dynamically.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" () "str" ((return (call "asDyn" ((con 2)))))) (expr (call "f" ()))))))))

;; conformance_suite/procedure_works.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" "int") ("y" dynamic)) dynamic ((return (bin-op - "y" "x")))) (assert (compare (call "f" ((con 2) (con 3))) ((is (con 1)))))))))))

;; conformance_suite/static_class_update_dynamic_field.py
(test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign ("c") (call "C2" ())) (assign ((attribute "c" "abc")) (con "foo"))))))))

;; conformance_suite/static_class_update_static_field.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign ("c") (call "C2" ())) (assign ((attribute "c" "x")) (con "foo")) (assert (compare (attribute "c" "x") ((is (con "foo")))))))))))

;; conformance_suite/test_assert_narrowing_debug.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "int" ((assert (call "isinstance" ("x" "int"))) (return (bin-op + "x" (con 1)))))))))))

;; conformance_suite/test_assert_narrowing_not_isinstance_optimized.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "str" ((assert (unary-op not (call "isinstance" ("x" "int")))) (return "x")))))))))

;; conformance_suite/test_assert_narrowing_optimized.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "object" ((assert (call "isinstance" ("x" "int"))) (return "x")))))))))

;; conformance_suite/test_aug_assign.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("l" dynamic)) dynamic ((aug-assign (subscript "l" (con 0)) + (con 1))))))))))

;; conformance_suite/test_augassign_inexact.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "something" () dynamic ((return (con 3)))) (function-def "t" () dynamic ((ann-assign "a" "int" (call "something" ())) (assign ("b") (con 0)) (aug-assign "b" + "a") (return "b")))))))))

;; conformance_suite/test_bind_boolop_type.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "f" (("self" dynamic)) "bool" ((return (con #t)))) (function-def "g" (("self" dynamic)) "bool" ((return (con #f)))) (function-def "x" (("self" dynamic)) "bool" ((return (bool-op and ((call (attribute "self" "f") ()) (call (attribute "self" "g") ())))))) (function-def "y" (("self" dynamic)) "bool" ((return (bool-op or ((call (attribute "self" "f") ()) (call (attribute "self" "g") ()))))))))))))))

;; conformance_suite/test_bind_none_compare_op.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (function-def "has_none" (("x" dynamic)) "bool" ((return (compare (con None) ((in "x")))))) (function-def "has_no_none" (("x" dynamic)) "bool" ((return (compare (con None) ((not-in "x"))))))))))))

;; conformance_suite/test_chkdict_literal.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (dict ())) (return "x")))))))))

;; conformance_suite/test_class_static_tpflag.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "A" () (pass))))))))

;; conformance_suite/test_class_unknown_decorator.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "dec" (("f" dynamic)) dynamic ((return "f"))) (class "C" () ((function-def "foo" (("self" dynamic)) "int" ((return (con 3)))) (function-def "f" (("self" dynamic)) dynamic ((return (call (attribute "self" "foo") ()))))))))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" "pydict" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x")))))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict_as_dict.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" "dict" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x")))))))))

;; conformance_suite/test_compile_checked_dict_len.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (return (call "len" ("x")))))))))))

;; conformance_suite/test_compile_checked_dict_opt_out_by_default.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x")))))))))

;; conformance_suite/test_compile_checked_dict_optional.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("str" (bin-op bit-or "str" (con None))))) ((dict (((con "x") (con None)) ((con "y") (con "z"))))))) (return "x")))))))))

;; conformance_suite/test_compile_checked_dict_reversed.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "D" ()) (con 42)) ((call "B" ()) (con 42))))))) (return "x")))))))))

;; conformance_suite/test_compile_checked_dict_type_specified.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "D" ()) (con 42))))))) (return "x")))))))))

;; conformance_suite/test_compile_checked_dict_with_annotation.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (dict (((call "B" ()) (con 42))))) (return "x")))))))))

;; conformance_suite/test_compile_dict_setitem.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (expr (call (attribute "x" "__setitem__") ((con 2) (con "def")))) (return "x")))))))))

;; conformance_suite/test_compile_dict_setitem_subscr.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (assign ((subscript "x" (con 2))) (con "def")) (return "x")))))))))

;; conformance_suite/test_compile_nested_dict.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))))) (assign ("y") (call (subscript "CheckedDict" (tuple ("int" (subscript "CheckedDict" (tuple ("B" "int")))))) ((dict (((con 42) "x")))))) (return "y")))))))))

;; conformance_suite/test_decorated_function_ignored.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (function-def "mydecorator" (("x" dynamic)) dynamic ((return "C"))) (function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((return (call "f" ()))))))))))

;; conformance_suite/test_dict_invoke.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (function-def "f" (("x" dynamic)) dynamic ((ann-assign "y" "pydict" "x") (return (call (attribute "y" "get") ((con "foo"))))))))))))

;; conformance_suite/test_dict_invoke_ret.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (function-def "g" () dynamic ((return (con None)))) (function-def "f" (("x" dynamic)) dynamic ((ann-assign "y" "pydict" "x") (assign ("z") (call (attribute "y" "get") ((con "foo")))) (assign ("z") (con None)) (return "z")))))))))

;; conformance_suite/test_generic_method_ret_type.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "MAP" (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) ((dict (((con "abc") (con "foo")) ((con "bar") (con None))))))) (function-def "f" (("x" "str")) (subscript "Optional" "str") ((return (call (attribute "MAP" "get") ("x")))))))))))

;; conformance_suite/test_incompat_override_init_okay.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "A" () ((function-def "__init__" (("self" dynamic)) (con None) (pass)))) (class "B" ("A") ((function-def "__init__" (("self" dynamic) ("x" "int")) (con None) (pass)))) (function-def "f" (("x" "A")) dynamic ((expr (call (attribute "x" "__init__") ()))))))))))

;; conformance_suite/test_inline_final.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("inline")) (import-from "typing" ("Final")) (ann-assign "Y" (subscript "Final" "int") (con 42)) (function-def "f" (("x" dynamic)) dynamic ((return (bin-op + "x" "Y")))) (function-def "g" () dynamic ((return (call "f" ((con 1))))))))))))

;; conformance_suite/test_inline_nested.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("inline")) (function-def "e" (("x" dynamic) ("y" dynamic)) dynamic ((return (bin-op + "x" "y")))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "e" ("x" (con 3)))))) (function-def "g" () dynamic ((return (call "f" ((con 1) (con 2))))))))))))

;; conformance_suite/test_inline_nested_arg.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("inline")) (function-def "e" (("x" dynamic) ("y" dynamic)) dynamic ((return (bin-op + "x" "y")))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "e" ("x" (con 3)))))) (function-def "g" (("a" dynamic) ("b" dynamic)) dynamic ((return (call "f" ("a" "b")))))))))))

;; conformance_suite/test_inline_recursive.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("inline")) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "f" ("x" "y"))))) (function-def "g" () dynamic ((return (call "f" ((con 1) (con 2))))))))))))

;; conformance_suite/test_invoke_all_extra_args.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "target" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic) ("g" dynamic)) dynamic ((return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * "a" (con 2)) (bin-op * "b" (con 3))) (bin-op * "c" (con 4))) (bin-op * "d" (con 5))) (bin-op * "e" (con 6))) (bin-op * "f" (con 7))) "g")))) (function-def "testfunc" () dynamic ((return (call "target" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6) (con 7))))))))))))

;; conformance_suite/test_invoke_all_reg_args.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "target" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic)) dynamic ((return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * "a" (con 2)) (bin-op * "b" (con 3))) (bin-op * "c" (con 4))) (bin-op * "d" (con 5))) (bin-op * "e" (con 6))) (bin-op * "f" (con 7)))))) (function-def "testfunc" () dynamic ((return (call "target" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6))))))))))))

;; conformance_suite/test_invoke_chkdict_method.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "dict_maker" () (subscript "CheckedDict" (tuple ("int" "int"))) ((return (call (subscript "CheckedDict" (tuple ("int" "int"))) ((dict (((con 2) (con 2))))))))) (function-def "func" () dynamic ((assign ("a") (call "dict_maker" ())) (return (call (attribute "a" "keys") ()))))))))))

;; conformance_suite/test_invoke_int_method.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "func" () dynamic ((assign ("a") (con 42)) (return (call (attribute "a" "bit_length") ()))))))))))

;; conformance_suite/test_invoke_method_non_static_base.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" ("Exception") ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))) (function-def "g" (("self" dynamic)) dynamic ((return (call (attribute "self" "f") ()))))))))))))

;; conformance_suite/test_invoke_str_method_arg.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "func" () dynamic ((assign ("a") (con "a b c")) (return (call (attribute "a" "split") ((con "a"))))))))))))

;; conformance_suite/test_invoke_strict_module_deep.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f0" () dynamic ((return (con 42)))) (function-def "f1" () dynamic ((return (call "f0" ())))) (function-def "f2" () dynamic ((return (call "f1" ())))) (function-def "f3" () dynamic ((return (call "f2" ())))) (function-def "f4" () dynamic ((return (call "f3" ())))) (function-def "f5" () dynamic ((return (call "f4" ())))) (function-def "f6" () dynamic ((return (call "f5" ())))) (function-def "f7" () dynamic ((return (call "f6" ())))) (function-def "f8" () dynamic ((return (call "f7" ())))) (function-def "f9" () dynamic ((return (call "f8" ())))) (function-def "f10" () dynamic ((return (call "f9" ())))) (function-def "f11" () dynamic ((return (call "f10" ())))) (function-def "g" () dynamic ((return (call "f11" ()))))))))))

;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f0" () dynamic ((return (con 42)))) (function-def "f1" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic) ("g" dynamic) ("h" dynamic)) dynamic ((class "C" () (pass)) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (call "f0" ()) "a") "b") "c") "d") "e") "f") "g") "h") (con 4))))) (function-def "f2" () dynamic ((return (call "f1" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6) (con 7) (con 8)))))) (function-def "f3" () dynamic ((return (call "f2" ())))) (function-def "f4" () dynamic ((return (call "f3" ())))) (function-def "f5" () dynamic ((return (call "f4" ())))) (function-def "f6" () dynamic ((return (call "f5" ())))) (function-def "f7" () dynamic ((return (call "f6" ())))) (function-def "f8" () dynamic ((return (call "f7" ())))) (function-def "f9" () dynamic ((return (call "f8" ())))) (function-def "f10" () dynamic ((return (call "f9" ())))) (function-def "f11" () dynamic ((return (call "f10" ())))) (function-def "g" () dynamic ((return (call "f11" ()))))))))))

;; conformance_suite/test_invoke_strict_module_pre_invoked.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((return (call "f" ()))))))))))

;; conformance_suite/test_max.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "max" ("a" "b")))))))))))

;; conformance_suite/test_max_stability.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "max" ("a" "b")))))))))))

;; conformance_suite/test_method_prologue_no_annotation.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" dynamic)) dynamic ((return (con 42))))))))))

;; conformance_suite/test_min.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "min" ("a" "b")))))))))))

;; conformance_suite/test_min_stability.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "min" ("a" "b")))))))))))

;; conformance_suite/test_module_subclass.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((function-def "__init__" (("self" dynamic)) dynamic ((ann-assign (attribute "self" "x") (subscript "Optional" "C") (con None))))))))))))

;; conformance_suite/test_no_narrow_to_dynamic.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((ann-assign "x" "int" (con 100)) (assign ("x") (call "f" ())) (return (call (attribute "x" "bit_length") ()))))))))))

;; conformance_suite/test_none_not.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "t" () "bool" ((assign ("x") (con None)) (if (unary-op not "x") ((return (con #t))) ((return (con #f))))))))))))

;; conformance_suite/test_override_bad_ret.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "B" () ((function-def "f" (("self" dynamic)) (con "B") ((return "self"))))) (function-def "f" (("x" "B")) dynamic ((return (call (attribute "x" "f") ()))))))))))

;; conformance_suite/test_override_okay.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "B" () ((function-def "f" (("self" dynamic)) (con "B") ((return "self"))))) (function-def "f" (("x" "B")) dynamic ((return (call (attribute "x" "f") ()))))))))))

;; conformance_suite/test_package_no_parent.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((function-def "f" (("self" dynamic)) dynamic ((return (con 42))))))))))))

;; conformance_suite/test_pydict_arg_annotation.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (function-def "f" (("d" (subscript "PyDict" (tuple ("str" "int"))))) "str" ((return (subscript "d" (con 3)))))))))))

;; conformance_suite/test_refine_or_expression.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" (("s" (subscript "Optional" "str"))) "str" ((return (bool-op or ("s" (con "hi"))))))))))))

;; conformance_suite/test_refine_or_expression_with_multiple_optionals.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" (("s1" (subscript "Optional" "str")) ("s2" (subscript "Optional" "str"))) "str" ((return (bool-op or ("s1" "s2" (con "hi"))))))))))))

;; conformance_suite/test_ret_type_cast.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (function-def "testfunc" (("x" "str") ("y" "str")) "bool" ((return (compare "x" ((== "y"))))))))))))

;; conformance_suite/test_slotification_decorated.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "_Inner" () (pass)) (function-def "something" (("klass" dynamic)) dynamic ((return "_Inner"))) (class "C" () ((function-def "f" (("self" dynamic)) dynamic (pass)))) (function-def "f" () dynamic ((return (call (attribute (call "C" ()) "f") ()))))))))))

;; conformance_suite/test_str_split.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "get_str" () "str" ((return (con "something here")))) (function-def "test" () "str" ((assign ((tuple ("a" "b"))) (call (attribute (call "get_str" ()) "split") ((con None) (con 1)))) (return "b")))))))))

;; conformance_suite/test_unknown_isinstance_bool_ret.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "__eq__" (("self" dynamic) ("other" "Any")) "bool" ((return (call "isinstance" ("other" "C")))))))))))))

;; conformance_suite/test_unknown_isinstance_narrows.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))))) (function-def "testfunc" (("x" dynamic)) dynamic ((if (call "isinstance" ("x" "C")) ((return (attribute "x" "x"))) ())))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "f" (("self" dynamic) ("other" dynamic)) "str" ((if (call "isinstance" ("other" (attribute "self" "__class__"))) ((return (attribute "other" "x"))) ()) (return (con ""))))))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr_dynamic.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "f" (("self" dynamic) ("other" dynamic) ("unknown" dynamic)) dynamic ((if (call "isinstance" ("other" (attribute "unknown" "__class__"))) ((return (attribute "other" "x"))) ()) (return (con ""))))))))))))

;; conformance_suite/test_unknown_isinstance_narrows_else_correct.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))))) (function-def "testfunc" (("x" dynamic)) dynamic ((if (call "isinstance" ("x" "C")) (pass) ((return (attribute "x" "x"))))))))))))

;; conformance_suite/test_unknown_issubclass_bool_ret.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "__eq__" (("self" dynamic) ("other" "Any")) "bool" ((return (call "issubclass" ((call "type" ("other")) "C")))))))))))))

;; conformance_suite/test_unknown_param_ann.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "__init__" (("self" dynamic) ("x" "str")) dynamic ((ann-assign (attribute "self" "x") "str" "x"))) (function-def "__eq__" (("self" dynamic) ("other" "Any")) "bool" ((return (con #f))))))))))))

;; conformance_suite/test_verify_lambda.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((assign ("x") (lambda (("x" dynamic)) "x")) (assign ("a") (call "x" ((con "hi"))))))))))

;; conformance_suite/test_visit_if_else.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((assign ("x") (con 0)) (if "x" (pass) ((function-def "f" () dynamic ((return (con 42))))))))))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "bool" (con #t)) (ann-assign "y" "int" "x")))))))
