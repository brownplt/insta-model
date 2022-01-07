#lang racket
(require redex/reduction-semantics)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")
(require "dynamics.rkt")
(require rackunit)

;; conformance_suite/CheckedDict_delete_bad_key.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "other")))))))))))

;; conformance_suite/CheckedDict_delete_checks_keys.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (delete (subscript (call "asDyn" ("x")) (con 42)))))))))))

;; conformance_suite/CheckedDict_delete_good_key.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "bar")))))))))))

;; conformance_suite/CheckedDict_delete_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (delete (subscript "x" (con "foo")))))))))))

;; conformance_suite/CheckedDict_delete_then_lookup.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (delete (subscript "x" (con "bar"))) (expr (subscript "x" (con "bar")))))))))))

;; conformance_suite/CheckedDict_from_bad_dict.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("d") (dict (((con 2) (con "a")) ((con 3) (con 4))))) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ("d")))))))))))

;; conformance_suite/CheckedDict_from_dict_literal_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (dict (((con 2) (con "a")))))))))))))

;; conformance_suite/CheckedDict_from_good_dict.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("d") (dict (((con 2) (con "a")) ((con 3) (con "b"))))) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ("d")))))))))))

;; conformance_suite/CheckedDict_from_nondict.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (call (subscript "CheckedDict" (tuple ("int" "str"))) ((con 42))))))))))))

;; conformance_suite/CheckedDict_insert.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (assign ((subscript "x" (con "new"))) (con 4))))))))))

;; conformance_suite/CheckedDict_insert_then_lookup.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict ())))) (assign ((subscript "x" (con "foo"))) (con 42)) (expr (subscript "x" (con "foo")))))))))))

;; conformance_suite/CheckedDict_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3)))))))))))))))

;; conformance_suite/CheckedDict_key_can_be_Optional.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "x" (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "int"))) (call (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "int"))) ((dict (((con "foo") (con 2)) ((con None) (con 3))))))) (assert (compare (subscript "x" (con None)) ((is (con 3)))))))))))))

;; conformance_suite/CheckedDict_lookup_checks_keys.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (expr (subscript (call "asDyn" ("x")) (con 42)))))))))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (expr (subscript "x" (con "foo")))))))))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (ann-assign "y" "int" (subscript "x" (con "foo")))))))))))

;; conformance_suite/CheckedDict_update.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2)) ((con "bar") (con 3))))))) (assign ((subscript "x" (con "bar"))) (con 4))))))))))

;; conformance_suite/CheckedDict_update_checks_keys.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign ((subscript (call "asDyn" ("x")) (con 42))) (con "bar"))))))))))

;; conformance_suite/CheckedDict_update_checks_values.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign ((subscript (call "asDyn" ("x")) (con "foo"))) (con "bar"))))))))))

;; conformance_suite/CheckedDict_update_key_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign ((subscript "x" (con "bar"))) (con 3))))))))))

;; conformance_suite/CheckedDict_update_then_lookup.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 2))))))) (assign ((subscript "x" (con "foo"))) (con 3)) (assert (compare (subscript "x" (con "foo")) ((is (con 3)))))))))))))

;; conformance_suite/CheckedDict_update_val_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" "int"))) (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "foo") (con 1))))))) (assign ((subscript "x" (con "bar"))) (con 2))))))))))

;; conformance_suite/CheckedDict_val_can_be_Optional.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "x" (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "int")))) (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "int")))) ((dict (((con "foo") (con 2)) ((con "bar") (con None))))))) (assert (compare (subscript "x" (con "bar")) ((is (con None)))))))))))))

;; conformance_suite/None_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" (con None) (con None))))))))))

;; conformance_suite/PyDict_delete_bad_key.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "other")))))))))))

;; conformance_suite/PyDict_delete_good_key.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "bar")))))))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (delete (subscript "x" (con "bar"))) (expr (subscript "x" (con "bar")))))))))))

;; conformance_suite/PyDict_insert.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (assign ((subscript "x" (con "new"))) (con "hello"))))))))))

;; conformance_suite/PyDict_insert_then_lookup.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict ())) (assign ((subscript "x" (con "foo"))) (con 42)) (expr (subscript "x" (con "foo")))))))))))

;; conformance_suite/PyDict_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2)))))))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (expr (subscript "x" (con "other")))))))))))

;; conformance_suite/PyDict_lookup_good_key.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (expr (subscript "x" (con "bar")))))))))))

;; conformance_suite/PyDict_to_CheckedDict_backward.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict" "PyDict")) (function-def "f" () dynamic ((return (call (subscript "CheckedDict" (tuple ("int" "int"))) ((dict ())))))) (ann-assign "x" "PyDict" (call "f" ()))))))))))

;; conformance_suite/PyDict_to_CheckedDict_forward.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "f" () dynamic ((return (dict ())))) (ann-assign "x" (subscript "CheckedDict" (tuple ("int" "int"))) (call "f" ()))))))))))

;; conformance_suite/PyDict_update.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con 1) (con "foo")) ((con "bar") (con 2))))) (assign ((subscript "x" (con "bar"))) (con "hello"))))))))))

;; conformance_suite/PyDict_update_then_lookup.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (ann-assign "x" "PyDict" (dict (((con "foo") (con 2))))) (assign ((subscript "x" (con "foo"))) (con 3)) (assert (compare (subscript "x" (con "foo")) ((is (con 3)))))))))))))

;; conformance_suite/bool_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "bool" (con #t))))))))))

;; conformance_suite/class_variables_declare_and_init.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int") (con 42))))))))))))

;; conformance_suite/class_variables_declare_only.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int"))))))))))))

;; conformance_suite/class_variables_may_shadow.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C1" () ((ann-assign "x" (subscript "ClassVar" "int")))) (assign ((attribute "C1" "x")) (con 2)) (class "C2" ("C1") ((ann-assign "x" (subscript "ClassVar" "int")))) (assign ((attribute "C2" "x")) (con 3)) (assert (compare (attribute "C1" "x") ((is (con 2))))) (assert (compare (attribute "C2" "x") ((is (con 3)))))))))))))

;; conformance_suite/class_variables_readable_at_instance_level.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int")))) (assign ((attribute "C" "x")) (con 42)) (assign ("obj") (call "C" ())) (assert (compare (attribute "obj" "x") ((is (con 42)))))))))))))

;; conformance_suite/class_variables_redeclare_in_subclass_same_type.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C1" () ((ann-assign "x" (subscript "ClassVar" "int")))) (class "C2" ("C1") ((ann-assign "x" (subscript "ClassVar" "int"))))))))))))

;; conformance_suite/class_variables_should_be_declared_with_ClassVar_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "int") (con 42)))) (assert (compare (attribute "C" "x") ((is (con 42)))))))))))))

;; conformance_suite/classes_are_not_first-class.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (function-def "checkExpect" (("cls" dynamic) ("obj" dynamic)) dynamic ((ann-assign "x" "cls" "obj") (return "x"))) (expr (call "checkExpect" ("C" (con 42))))))))))))

;; conformance_suite/classes_work.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (assign ("o") (call "C" ()))))))))))

;; conformance_suite/downcast_C2_to_C1_neg.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((class "C1" () (pass)) (class "C2" ("C1") (pass)) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "C1" (call "C1" ())) (ann-assign "y" "C2" (call "asDyn" ("x")))))))))))

;; conformance_suite/downcast_C2_to_C1_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () (pass)) (class "C2" ("C1") (pass)) (function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "C1" (call "C2" ())) (ann-assign "y" "C2" (call "asDyn" ("x")))))))))))

;; conformance_suite/downcast_int_to_bool_neg.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "int" (con 2)) (ann-assign "y" "bool" (call "asDyn" ("x")))))))))))

;; conformance_suite/downcast_int_to_bool_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "int" (con #t)) (ann-assign "y" "bool" (call "asDyn" ("x")))))))))))

;; conformance_suite/empty_program.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ())))))))

;; conformance_suite/field_init.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((ann-assign "x" "str") (function-def "__init__" (("self" dynamic) ("arg" dynamic)) (con None) ((assign ((attribute "self" "x")) "arg"))))) (try-except-else-finally ((assign ("o") (call "C" ((con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/field_update.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((ann-assign "x" "str") (function-def "__init__" (("self" dynamic) ("arg" dynamic)) (con None) ((assign ((attribute "self" "x")) "arg"))))) (assign ("o") (call "C" ((con "foo")))) (try-except-else-finally ((function-def "dyn_int" () dynamic ((return (con 42)))) (assign ((attribute "o" "x")) (call "dyn_int" ()))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/for-loop_basic.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "fact" (("n" "int")) "int" ((ann-assign "o" "int" (con 1)) (for "i" (call "range" ("n")) ((aug-assign "o" * (bin-op + "i" (con 1)))) ()) (return "o"))) (assert (compare (call "fact" ((con 5))) ((is (con 120)))))))))))))

;; conformance_suite/for-loop_else_break.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((for "i" (list ((con 2))) (break) ((assign ("i") (con 3)))) (assert (compare "i" ((is (con 2)))))))))))))

;; conformance_suite/for-loop_else_nonbreak.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((for "i" (list ((con 2))) (pass) ((assign ("i") (con 3)))) (assert (compare "i" ((is (con 3)))))))))))))

;; conformance_suite/ht_test_checked_dict.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (import-from "__static__" ("CheckedDict")) (ann-assign "x" "Any" (call (subscript "CheckedDict" (tuple ("str" "str"))) ())) (assign ((subscript "x" (con "abc"))) (con "foo")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ())) (assign ((subscript "x" (con "abc"))) (con 42)) (assign ("x") (call (subscript "CheckedDict" (tuple ("int" "str"))) ())) (assign ((subscript "x" (con 42))) (con "abc"))))))))))

;; conformance_suite/ht_test_checked_dict_bad_ctor.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (try-except-else-finally ((expr (call (subscript "CheckedDict" (tuple ("str" "str"))) ((con None))))) ((except-handler "Exception" None (pass))) ((raise (call "Exception" ((con "Should fail."))))) ())))))))))

;; conformance_suite/ht_test_checked_dict_clear.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "str"))) ((dict (((con "x") (con "abc"))))))) (expr (call (attribute "x" "clear") ())) (assert (compare "x" ((== (dict ())))))))))))))

;; conformance_suite/ht_test_checked_dict_copy.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "str"))) ((dict (((con "x") (con "abc"))))))) (assert (compare (call "type" ("x")) ((== (subscript "CheckedDict" (tuple ("str" "str"))))))) (assert (compare "x" ((== (dict (((con "x") (con "abc"))))))))))))))))

;; conformance_suite/ht_test_checked_dict_errors.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "as_dyn" (("x" dynamic)) dynamic ((return "x"))) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (try-except-else-finally ((expr (call (attribute "x" "get") ((call "as_dyn" ((con 100))))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (try-except-else-finally ((expr (call (attribute "x" "get") ((con "x") (call "as_dyn" ((con "abc"))))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/ht_test_checked_dict_fromkeys_bad_types.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (try-except-else-finally ((expr (call (attribute (subscript "CheckedDict" (tuple ("str" "int"))) "fromkeys") ((list ((con 2))) (con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (try-except-else-finally ((expr (call (attribute (subscript "CheckedDict" (tuple ("str" "int"))) "fromkeys") ((con "abc") (call "object" ()))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (try-except-else-finally ((expr (call (attribute (subscript "CheckedDict" (tuple ("str" "int"))) "fromkeys") ((con "abc"))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/ht_test_checked_dict_get.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (assert (compare (call (attribute "x" "get") ((con "x"))) ((== (con 2))))) (assert (compare (call (attribute "x" "get") ((con "y") (con 100))) ((== (con 100)))))))))))))

;; conformance_suite/ht_test_checked_dict_getitem.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (assert (compare (call (attribute "x" "__getitem__") ((con "x"))) ((== (con 2)))))))))))))

;; conformance_suite/ht_test_checked_dict_items.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (assert (compare (call "list" ((call (attribute "x" "items") ()))) ((== (list ((tuple ((con "x") (con 2))))))))) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2)) ((con "y") (con 3))))))) (assert (compare (call "list" ((call (attribute "x" "items") ()))) ((== (list ((tuple ((con "x") (con 2))) (tuple ((con "y") (con 3)))))))))))))))))

;; conformance_suite/ht_test_checked_dict_keys.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (assert (compare (call "list" ((call (attribute "x" "keys") ()))) ((== (list ((con "x"))))))) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2)) ((con "y") (con 3))))))) (assert (compare (call "list" ((call (attribute "x" "keys") ()))) ((== (list ((con "x") (con "y")))))))))))))))

;; conformance_suite/ht_test_checked_dict_nonoptional.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Any" "Optional")) (function-def "as_dyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "Any" (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) ())) (try-except-else-finally ((assign ((subscript "x" (call "as_dyn" ((con None))))) (con "abc"))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assign ("x") (call (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "str"))) ())) (try-except-else-finally ((assign ((subscript "x" (con "abc"))) (call "as_dyn" ((con None))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/ht_test_checked_dict_optional.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any" "Optional")) (import-from "__static__" ("CheckedDict")) (ann-assign "x" "Any" (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) ())) (assign ((subscript "x" (con "abc"))) (con None)) (assign ("x") (call (subscript "CheckedDict" (tuple ((subscript "Optional" "str") "str"))) ())) (assign ((subscript "x" (con None))) (con "abc"))))))))))

;; conformance_suite/ht_test_checked_dict_pop.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (assign ("y") (call (attribute "x" "pop") ((con "x")))) (assert (compare "y" ((== (con 2))))) (try-except-else-finally ((expr (call (attribute "x" "pop") ((con "z"))))) ((except-handler "KeyError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/ht_test_checked_dict_popitem.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2))))))) (assign ("y") (call (attribute "x" "popitem") ())) (assert (compare "y" ((== (tuple ((con "x") (con 2))))))) (try-except-else-finally ((expr (call (attribute "x" "popitem") ()))) ((except-handler "KeyError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/ht_test_checked_dict_setdefault.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "str"))) ())) (expr (call (attribute "x" "setdefault") ((con "abc") (con "foo")))) (assert (compare "x" ((== (dict (((con "abc") (con "foo"))))))))))))))))

;; conformance_suite/ht_test_checked_dict_setdefault_bad_values.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "as_dyn" (("x" dynamic)) dynamic ((return "x"))) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ())) (try-except-else-finally ((expr (call (attribute "x" "setdefault") ((con "abc") (call "as_dyn" ((con "abc"))))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ()))))) (try-except-else-finally ((expr (call (attribute "x" "setdefault") ((call "as_dyn" ((con 42))) (con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ())))))))))))))

;; conformance_suite/ht_test_checked_dict_types_enforced.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (import-from "__static__" ("CheckedDict")) (function-def "as_dyn" (("x" dynamic)) dynamic ((return "x"))) (ann-assign "x" "Any" (call (subscript "CheckedDict" (tuple ("str" "str"))) ())) (try-except-else-finally ((assign ((subscript "x" (call "as_dyn" ((con 42))))) (con "abc"))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ()))))) (try-except-else-finally ((assign ((subscript "x" (con "abc"))) (call "as_dyn" ((con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ()))))) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ())) (try-except-else-finally ((assign ((subscript "x" (call "as_dyn" ((con 42))))) (con 42))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ()))))) (try-except-else-finally ((assign ((subscript "x" (con "abc"))) (call "as_dyn" ((con "abc"))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ())))))))))))))

;; conformance_suite/ht_test_checked_dict_update.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "str"))) ((dict (((con "x") (con "abc"))))))) (expr (call (attribute "x" "update") ((dict (((con "y") (con "foo"))))))) (assert (compare "x" ((== (dict (((con "x") (con "abc")) ((con "y") (con "foo")))))))) (expr (call (attribute "x" "update") ((dict (((con "z") (con "bar"))))))) (assert (compare "x" ((== (dict (((con "x") (con "abc")) ((con "y") (con "foo")) ((con "z") (con "bar"))))))))))))))))

;; conformance_suite/ht_test_checked_dict_update_bad_type.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ())) (try-except-else-finally ((expr (call (attribute "x" "update") ((dict (((con "x") (con "abc")))))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ()))))) (try-except-else-finally ((expr (call (attribute "x" "update") ((dict (((con "x") (con "abc")))))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (try-except-else-finally ((expr (call (attribute "x" "update") ((dict (((con 24) (con 42)))))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()) (assert (compare "x" ((== (dict ())))))))))))))

;; conformance_suite/ht_test_checked_dict_values.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (assign ("x") (call (subscript "CheckedDict" (tuple ("str" "int"))) ((dict (((con "x") (con 2)) ((con "y") (con 3))))))) (assert (compare (call "list" ((call (attribute "x" "values") ()))) ((== (list ((con 2) (con 3)))))))))))))))

;; conformance_suite/ht_test_compile_checked_dict_shadowcode.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))))) (return "x"))) (assign ("test") "testfunc") (for "i" (call "range" ((con 200))) ((assert (compare (call "type" ((call "test" ()))) ((== (subscript "CheckedDict" (tuple ("B" "int")))))))) ())))))))))

;; conformance_suite/ht_test_compile_dict_get_2.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))))) (return "x"))) (assert (compare (call "type" ((call "testfunc" ()))) ((== (subscript "CheckedDict" (tuple ("B" "int")))))))))))))))

;; conformance_suite/ht_test_method_prologue_shadowcode.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" dynamic) ("y" "str")) dynamic ((return (con 42)))) (function-def "main" (("f" dynamic)) dynamic ((for "i" (call "range" ((con 100))) ((assert (compare (call "f" ((con "abc") (con "abc"))) ((== (con 42)))))) ()) (try-except-else-finally ((expr (call "f" ((con "abc") (con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("f")))))))))))

;; conformance_suite/ht_test_seq_repeat_inexact_list.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("List")) (function-def "f" (("l" (subscript "List" "int"))) dynamic ((return (bin-op * "l" (con 2))))) (assert (compare (call "f" ((list ((con 1) (con 2))))) ((== (list ((con 1) (con 2) (con 1) (con 2))))))) (class "MyList" ("list") ((function-def "__mul__" (("self" dynamic) ("other" dynamic)) dynamic ((return (con "RESULT")))))) (assert (compare (call "f" ((call "MyList" ((list ((con 1) (con 2))))))) ((== (con "RESULT")))))))))))))

;; conformance_suite/ht_test_seq_repeat_inexact_num.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("num" "int")) dynamic ((return (bin-op * "num" (list ((con 1) (con 2))))))) (assert (compare (call "f" ((con 2))) ((== (list ((con 1) (con 2) (con 1) (con 2))))))) (class "MyInt" ("int") ((function-def "__mul__" (("self" dynamic) ("other" dynamic)) dynamic ((return (con "RESULT")))))) (assert (compare (call "f" ((call "MyInt" ((con 2))))) ((== (con "RESULT")))))))))))))

;; conformance_suite/ht_test_typed_field_deleted_attr.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((ann-assign "x" "str") (function-def "__init__" (("self" dynamic) ("value" "str")) dynamic ((assign ((attribute "self" "x")) "value"))))) (assign ("a") (call "C" ((con "abc")))) (delete (attribute "a" "x")) (try-except-else-finally ((expr (attribute "a" "x"))) ((except-handler "AttributeError" None (pass))) ((raise (call "Exception" ()))) ())))))))))

;; conformance_suite/ht_test_verify_arg_dynamic_type.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "x" (("v" "str")) dynamic ((return (con "abc")))) (function-def "y" (("v" dynamic)) dynamic ((return (call "x" ("v"))))) (try-except-else-finally ((expr (call "y" ((con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ((con "y(42)"))))) ()) (assert (compare (call "y" ((con "foo"))) ((== (con "abc")))))))))))))

;; conformance_suite/instance_creation.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((ann-assign "x" "str") (function-def "__init__" (("self" dynamic) ("arg" dynamic)) (con None) ((assign ((attribute "self" "x")) "arg"))))) (assign ("o") (call "C" ((con "foo")))) (assert (compare (attribute "o" "x") ((== (con "foo")))))))))))))

;; conformance_suite/int_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "int" (con 42))))))))))

;; conformance_suite/iterate-list.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((assign ("l") (list ())) (for "x" (list ((con 1) (con 2) (con 3))) ((expr (call (attribute "l" "append") ("x")))) ()) (assert (compare "l" ((== (list ((con 1) (con 2) (con 3)))))))))))))))

;; conformance_suite/iterate-str.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((assign ("l") (list ())) (for "x" (con "abc") ((expr (call (attribute "l" "append") ("x")))) ()) (assert (compare "l" ((== (list ((con "a") (con "b") (con "c")))))))))))))))

;; conformance_suite/list_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "list" (list ()))))))))))

;; conformance_suite/method_from_def.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((function-def "m" (("self" dynamic)) dynamic ((return (con 2)))))) (assign ("obj") (call "C" ())) (assert (compare (call (attribute "obj" "m") ()) ((is (con 2)))))))))))))

;; conformance_suite/method_from_lambda.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any" "ClassVar")) (class "C" () ((ann-assign "m" (subscript "ClassVar" "Any") (lambda (("self" dynamic)) (con 2))))) (assign ("obj") (call "C" ())) (assert (compare (call (attribute "obj" "m") ()) ((is (con 2)))))))))))))

;; conformance_suite/method_generative.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any" "ClassVar")) (class "C" () ((ann-assign "m1" (subscript "ClassVar" "Any") (lambda (("self" dynamic)) (con 2))) (function-def "m2" (("self" dynamic)) dynamic ((return (con 3)))))) (assign ("obj") (call "C" ())) (assert (compare (attribute "obj" "m1") ((is-not (attribute "obj" "m1"))))) (assert (compare (attribute "obj" "m2") ((is-not (attribute "obj" "m2")))))))))))))

;; conformance_suite/method_override_dynamic.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () ((function-def "m" (("self" dynamic)) dynamic ((return (con 2)))))) (class "C2" ("C1") ((function-def "m" (("self" dynamic)) dynamic ((return (con 3)))))) (function-def "f" () dynamic ((return (call "C2" ())))) (assign ("o") (call "f" ())) (assert (compare (call (attribute "o" "m") ()) ((is (con 3)))))))))))))

;; conformance_suite/method_override_exact.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () ((function-def "m" (("self" dynamic)) dynamic ((return (con 2)))))) (class "C2" ("C1") ((function-def "m" (("self" dynamic)) dynamic ((return (con 3)))))) (assign ("o") (call "C2" ())) (assert (compare (call (attribute "o" "m") ()) ((is (con 3)))))))))))))

;; conformance_suite/method_override_inexact.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () ((function-def "m" (("self" dynamic)) dynamic ((return (con 2)))))) (class "C2" ("C1") ((function-def "m" (("self" dynamic)) dynamic ((return (con 3)))))) (function-def "f" () "C1" ((return (call "C2" ())))) (assign ("o") (call "f" ())) (assert (compare (call (attribute "o" "m") ()) ((is (con 3)))))))))))))

;; conformance_suite/methods_can_be_declared_as_class_variables.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("ClassVar" "Any")) (class "C" () ((ann-assign "x" (subscript "ClassVar" "Any") (lambda (("self" dynamic) ("n" dynamic)) (bin-op + "n" (con 1)))))) (assign ("o") (call "C" ())) (assert (compare (call (attribute "o" "x") ((con 2))) ((is (con 3)))))))))))))

;; conformance_suite/object_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "object" (call "object" ()))))))))))

;; conformance_suite/optional_is_inhabitable_none.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (ann-assign "x" (subscript "Optional" "int") (con None))))))))))

;; conformance_suite/optional_is_inhabitable_none_rt.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" () dynamic ((return (con None)))) (ann-assign "x" (subscript "Optional" "int") (call "f" ()))))))))))

;; conformance_suite/optional_is_inhabitable_nonnone.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (ann-assign "x" (subscript "Optional" "int") (con 42))))))))))

;; conformance_suite/optional_is_inhabitable_nonnone_rt.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" () dynamic ((return (con 42)))) (ann-assign "x" (subscript "Optional" "int") (call "f" ()))))))))))

;; conformance_suite/optional_is_inhabitable_other_rt.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" () dynamic ((return (con "foo")))) (ann-assign "x" (subscript "Optional" "int") (call "f" ()))))))))))

;; conformance_suite/optional_refine_and.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "expect_int" (("i" "int")) (con None) ((return (con None)))) (function-def "f" (("x" (subscript "Optional" "int"))) (con None) ((return (bool-op and ("x" (call "expect_int" ("x"))))))) (assert (compare (call "f" ((con None))) ((is (con None))))) (assert (compare (call "f" ((con 42))) ((is (con None)))))))))))))

;; conformance_suite/optional_refine_if.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) "int" ((if "x" ((return "x")) ((return (con 42)))))) (assert (compare (call "f" ((con 2))) ((is (con 2))))) (assert (compare (call "f" ((con None))) ((is (con 42)))))))))))))

;; conformance_suite/optional_refine_is_None.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" (("x" (subscript "Optional" "int"))) "int" ((if (compare "x" ((is (con None)))) ((return (con 42))) ((return "x"))))) (assert (compare (call "f" ((con 2))) ((is (con 2))))) (assert (compare (call "f" ((con None))) ((is (con 42)))))))))))))

;; conformance_suite/override_instance_method_codomain_gain_precision.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C1" () ((function-def "m" (("self" dynamic)) "Any" ((return (con 2)))))) (class "C2" ("C1") ((function-def "m" (("self" dynamic)) "int" ((return (con 3))))))))))))))

;; conformance_suite/override_instance_method_contravariant_inputs_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (class "D" ("C") (pass)) (class "A" () ((function-def "m" (("self" dynamic) ("x" "D")) (con None) ((return (con None)))))) (class "B" ("A") ((function-def "m" (("self" dynamic) ("x" "C")) (con None) ((return (con None))))))))))))))

;; conformance_suite/override_instance_method_covariant_output_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () (pass)) (class "D" ("C") (pass)) (class "A" () ((function-def "m" (("self" dynamic)) "C" ((return (call "C" ())))))) (class "B" ("A") ((function-def "m" (("self" dynamic)) "D" ((return (call "D" ()))))))))))))))

;; conformance_suite/override_instance_method_domain_lose_precision.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C1" () ((function-def "m" (("self" dynamic) ("x" "int")) dynamic ((return (con 2)))))) (class "C2" ("C1") ((function-def "m" (("self" dynamic) ("x" "Any")) dynamic ((return (con 3))))))))))))))

;; conformance_suite/procedure_check_argument_type_dynamically.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" (("x" "int")) dynamic (pass)) (expr (call "f" ((call "asDyn" ((con "foo"))))))))))))))

;; conformance_suite/procedure_check_arity_dynamically.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic (pass)) (expr (call (call "asDyn" ("f")) ((con 2))))))))))))

;; conformance_suite/procedure_check_return_type_dynamically.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((function-def "asDyn" (("x" dynamic)) dynamic ((return "x"))) (function-def "f" () "str" ((return (call "asDyn" ((con 2)))))) (expr (call "f" ()))))))))))

;; conformance_suite/procedure_works.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" "int") ("y" dynamic)) dynamic ((return (bin-op - "y" "x")))) (assert (compare (call "f" ((con 2) (con 3))) ((is (con 1)))))))))))))

;; conformance_suite/static_class_update_dynamic_field.py
(check-not-exn (lambda () (test-match SP-dynamics (error any) (term (calc (compile-program (desugar-program ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign ("c") (call "C2" ())) (assign ((attribute "c" "abc")) (con "foo"))))))))))

;; conformance_suite/static_class_update_static_field.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () ((ann-assign "x" "str"))) (class "C2" ("C1") ((ann-assign "y" "int"))) (assign ("c") (call "C2" ())) (assign ((attribute "c" "x")) (con "foo")) (assert (compare (attribute "c" "x") ((is (con "foo")))))))))))))

;; conformance_suite/str_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "str" (con "hello"))))))))))

;; conformance_suite/test_assert_narrowing_debug.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "int" ((assert (call "isinstance" ("x" "int"))) (return (bin-op + "x" (con 1))))) (function-def "main" (("foo" dynamic)) dynamic ((assert (compare (call "foo" ((con 1))) ((== (con 2))))) (try-except-else-finally ((expr (call "foo" ((con "a"))))) ((except-handler "AssertionError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("foo")))))))))))

;; conformance_suite/test_assert_narrowing_not_isinstance_optimized.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "str" ((assert (unary-op not (call "isinstance" ("x" "int")))) (return "x"))) (function-def "main" (("foo" dynamic)) dynamic ((assert (compare (call "foo" ((con "abc"))) ((== (con "abc"))))))) (expr (call "main" ("foo")))))))))))

;; conformance_suite/test_assert_narrowing_optimized.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "foo" (("x" (bin-op bit-or "int" "str"))) "object" ((assert (call "isinstance" ("x" "int"))) (return "x"))) (function-def "main" (("foo" dynamic)) dynamic ((assert (compare (call "foo" ((con 1))) ((== (con 1))))) (try-except-else-finally ((expr (call "foo" ((con "a"))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("foo")))))))))))

;; conformance_suite/test_aug_assign.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("l" dynamic)) dynamic ((aug-assign (subscript "l" (con 0)) + (con 1)))) (function-def "main" (("f" dynamic)) dynamic ((assign ("l") (list ((con 1)))) (expr (call "f" ("l"))) (assert (compare (subscript "l" (con 0)) ((== (con 2))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_augassign_inexact.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "something" () dynamic ((return (con 3)))) (function-def "t" () dynamic ((ann-assign "a" "int" (call "something" ())) (assign ("b") (con 0)) (aug-assign "b" + "a") (return "b"))) (function-def "main" (("t" dynamic)) dynamic ((assert (compare (call "t" ()) ((== (con 3))))))) (expr (call "main" ("t")))))))))))

;; conformance_suite/test_bind_boolop_type.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (class "C" () ((function-def "f" (("self" dynamic)) "bool" ((return (con #t)))) (function-def "g" (("self" dynamic)) "bool" ((return (con #f)))) (function-def "x" (("self" dynamic)) "bool" ((return (bool-op and ((call (attribute "self" "f") ()) (call (attribute "self" "g") ())))))) (function-def "y" (("self" dynamic)) "bool" ((return (bool-op or ((call (attribute "self" "f") ()) (call (attribute "self" "g") ())))))))) (function-def "main" (("C" dynamic)) dynamic ((assign ("c") (call "C" ())) (assert (compare (call (attribute "c" "x") ()) ((== (con #f))))) (assert (compare (call (attribute "c" "y") ()) ((== (con #t))))))) (expr (call "main" ("C")))))))))))

;; conformance_suite/test_check_args.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "use" (("i" "object")) "object" ((return "i"))) (function-def "outer" (("x" "int")) "object" ((function-def "inner" () (con None) ((expr (call "use" ("x"))))) (return (call "use" ("x"))))) (function-def "main" (("outer" dynamic)) dynamic ((assert (compare (call "outer" ((con 1))) ((== (con 1))))))) (expr (call "main" ("outer")))))))))))

;; conformance_suite/test_check_args_2.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "use" (("i" "object")) "object" ((return "i"))) (function-def "outer" (("x" "int") ("y" "str")) "object" ((function-def "inner" () (con None) ((expr (call "use" ("x"))) (expr (call "use" ("y"))))) (expr (call "use" ("x"))) (return (call "use" ("y"))))) (function-def "main" (("outer" dynamic)) dynamic ((assert (compare (call "outer" ((con 1) (con "yo"))) ((== (con "yo"))))))) (expr (call "main" ("outer")))))))))))

;; conformance_suite/test_check_args_3.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "use" (("i" "object")) "object" ((return "i"))) (function-def "outer" (("x" "int") ("y" "str")) "object" ((function-def "inner" () (con None) ((expr (call "use" ("y"))))) (expr (call "use" ("x"))) (return (call "use" ("y"))))) (function-def "main" (("outer" dynamic)) dynamic ((assert (compare (call "outer" ((con 1) (con "yo"))) ((== (con "yo"))))))) (expr (call "main" ("outer")))))))))))

;; conformance_suite/test_chkdict_literal.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("int" "str"))) (dict ())) (return "x"))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "type" ((call "f" ()))) ((== (subscript "CheckedDict" (tuple ("int" "str"))))))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" "pydict" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x"))) (function-def "main" (("test" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== "dict")))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict_as_dict.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" "dict" (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x"))) (function-def "main" (("test" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== "dict")))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_compile_checked_dict_len.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("int" "str"))) ((dict (((con 1) (con "abc"))))))) (return (call "len" ("x"))))) (function-def "main" (("test" dynamic)) dynamic ((assert (compare (call "test" ()) ((== (con 1))))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_compile_checked_dict_opt_out_by_default.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))) (return "x"))) (function-def "main" (("test" dynamic) ("B" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== "dict")))))) (expr (call "main" ("testfunc" "B")))))))))))

;; conformance_suite/test_compile_checked_dict_optional.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("str" (bin-op bit-or "str" (con None))))) ((dict (((con "x") (con None)) ((con "y") (con "z"))))))) (return "x"))) (function-def "main" (("f" dynamic)) dynamic ((assign ("x") (call "f" ())) (assign ((subscript "x" (con "z"))) (con None)) (assert (compare (call "type" ("x")) ((== (subscript "CheckedDict" (tuple ("str" (bin-op bit-or "str" (con None))))))))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_compile_checked_dict_reversed.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "D" ()) (con 42)) ((call "B" ()) (con 42))))))) (return "x"))) (function-def "main" (("test" dynamic) ("B" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== (subscript "CheckedDict" (tuple ("B" "int"))))))))) (expr (call "main" ("testfunc" "B")))))))))))

;; conformance_suite/test_compile_checked_dict_type_specified.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "D" ()) (con 42))))))) (return "x"))) (function-def "main" (("test" dynamic) ("B" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== (subscript "CheckedDict" (tuple ("B" "int"))))))))) (expr (call "main" ("testfunc" "B")))))))))))

;; conformance_suite/test_compile_checked_dict_with_annotation.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (function-def "testfunc" () dynamic ((ann-assign "x" (subscript "CheckedDict" (tuple ("B" "int"))) (dict (((call "B" ()) (con 42))))) (return "x"))) (function-def "main" (("test" dynamic) ("B" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== (subscript "CheckedDict" (tuple ("B" "int"))))))))) (expr (call "main" ("testfunc" "B")))))))))))

;; conformance_suite/test_compile_nested_dict.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (class "B" () (pass)) (class "D" ("B") (pass)) (function-def "testfunc" () dynamic ((assign ("x") (call (subscript "CheckedDict" (tuple ("B" "int"))) ((dict (((call "B" ()) (con 42)) ((call "D" ()) (con 42))))))) (assign ("y") (call (subscript "CheckedDict" (tuple ("int" (subscript "CheckedDict" (tuple ("B" "int")))))) ((dict (((con 42) "x")))))) (return "y"))) (function-def "main" (("test" dynamic) ("B" dynamic)) dynamic ((assert (compare (call "type" ((call "test" ()))) ((== (subscript "CheckedDict" (tuple ("int" (subscript "CheckedDict" (tuple ("B" "int")))))))))))) (expr (call "main" ("testfunc" "B")))))))))))

;; conformance_suite/test_dict_invoke.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (function-def "f" (("x" dynamic)) dynamic ((ann-assign "y" "pydict" "x") (return (call (attribute "y" "get") ((con "foo")))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((dict ()))) ((== (con None))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_dict_invoke_ret.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("pydict")) (function-def "g" () dynamic ((return (con None)))) (function-def "f" (("x" dynamic)) dynamic ((ann-assign "y" "pydict" "x") (assign ("z") (call (attribute "y" "get") ((con "foo")))) (assign ("z") (con None)) (return "z"))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((dict ()))) ((== (con None))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_for_iter_unchecked_get.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("l") (list ((con 1) (con 2) (con 3)))) (assign ("acc") (list ())) (for "x" "l" ((expr (call (attribute "acc" "append") ("x")))) ()) (return "acc"))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ()) ((== (list ((con 1) (con 2) (con 3))))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_generic_method_ret_type.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (ann-assign "MAP" (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) (call (subscript "CheckedDict" (tuple ("str" (subscript "Optional" "str")))) ((dict (((con "abc") (con "foo")) ((con "bar") (con None))))))) (function-def "f" (("x" "str")) (subscript "Optional" "str") ((return (call (attribute "MAP" "get") ("x"))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con "abc"))) ((== (con "foo"))))) (assert (compare (call "f" ((con "bar"))) ((== (con None))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_inline_nested.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("inline")) (function-def "e" (("x" dynamic) ("y" dynamic)) dynamic ((return (bin-op + "x" "y")))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "e" ("x" (con 3)))))) (function-def "g" () dynamic ((return (call "f" ((con 1) (con 2)))))) (function-def "main" (("g" dynamic)) dynamic ((assert (compare (call "g" ()) ((== (con 4))))))) (expr (call "main" ("g")))))))))))

;; conformance_suite/test_inline_nested_arg.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("inline")) (function-def "e" (("x" dynamic) ("y" dynamic)) dynamic ((return (bin-op + "x" "y")))) (function-def "f" (("x" dynamic) ("y" dynamic)) dynamic ((return (call "e" ("x" (con 3)))))) (function-def "g" (("a" dynamic) ("b" dynamic)) dynamic ((return (call "f" ("a" "b"))))) (function-def "main" (("g" dynamic)) dynamic ((assert (compare (call "g" ((con 1) (con 2))) ((== (con 4))))))) (expr (call "main" ("g")))))))))))

;; conformance_suite/test_invoke_all_extra_args.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "target" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic) ("g" dynamic)) dynamic ((return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * "a" (con 2)) (bin-op * "b" (con 3))) (bin-op * "c" (con 4))) (bin-op * "d" (con 5))) (bin-op * "e" (con 6))) (bin-op * "f" (con 7))) "g")))) (function-def "testfunc" () dynamic ((return (call "target" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6) (con 7)))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ()) ((== (con 119))))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_invoke_all_reg_args.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "target" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic)) dynamic ((return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * "a" (con 2)) (bin-op * "b" (con 3))) (bin-op * "c" (con 4))) (bin-op * "d" (con 5))) (bin-op * "e" (con 6))) (bin-op * "f" (con 7)))))) (function-def "testfunc" () dynamic ((return (call "target" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6)))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ()) ((== (con 112))))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_invoke_base_inited.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "B" () ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))))) (assign ("X") (call (attribute (call "B" ()) "f") ())) (class "D" ("B") ((function-def "g" (("self" dynamic)) dynamic ((return (con 100)))))) (function-def "g" (("x" "D")) dynamic ((return (call (attribute "x" "g") ())))) (function-def "main" (("d" dynamic)) dynamic ((assert (compare "X" ((== (con 42))))) (assert (compare (call "g" ("d")) ((== (con 100))))))) (expr (call "main" ((call "D" ()))))))))))))

;; conformance_suite/test_invoke_chkdict_method.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (function-def "dict_maker" () (subscript "CheckedDict" (tuple ("int" "int"))) ((return (call (subscript "CheckedDict" (tuple ("int" "int"))) ((dict (((con 2) (con 2))))))))) (function-def "func" () dynamic ((assign ("a") (call "dict_maker" ())) (return (call (attribute "a" "keys") ())))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "list" ((call "f" ()))) ((== (list ((con 2))))))))) (expr (call "main" ("func")))))))))))

;; conformance_suite/test_invoke_int_method.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "func" () dynamic ((assign ("a") (con 42)) (return (call (attribute "a" "bit_length") ())))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ()) ((== (con 6))))))) (expr (call "main" ("func")))))))))))

;; conformance_suite/test_invoke_method_non_static_base.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" ("Exception") ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))) (function-def "g" (("self" dynamic)) dynamic ((return (call (attribute "self" "f") ())))))) (function-def "main" (("C" dynamic)) dynamic ((assert (compare (call (attribute (call "C" ()) "g") ()) ((== (con 42))))))) (expr (call "main" ("C")))))))))))

;; conformance_suite/test_invoke_str_method_arg.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "func" () dynamic ((assign ("a") (con "a b c")) (return (call (attribute "a" "split") ((con "a")))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ()) ((== (list ((con "") (con " b c"))))))))) (expr (call "main" ("func")))))))))))

;; conformance_suite/test_invoke_strict_module_deep.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f0" () dynamic ((return (con 42)))) (function-def "f1" () dynamic ((return (call "f0" ())))) (function-def "f2" () dynamic ((return (call "f1" ())))) (function-def "f3" () dynamic ((return (call "f2" ())))) (function-def "f4" () dynamic ((return (call "f3" ())))) (function-def "f5" () dynamic ((return (call "f4" ())))) (function-def "f6" () dynamic ((return (call "f5" ())))) (function-def "f7" () dynamic ((return (call "f6" ())))) (function-def "f8" () dynamic ((return (call "f7" ())))) (function-def "f9" () dynamic ((return (call "f8" ())))) (function-def "f10" () dynamic ((return (call "f9" ())))) (function-def "f11" () dynamic ((return (call "f10" ())))) (function-def "g" () dynamic ((return (call "f11" ())))) (function-def "main" (("g" dynamic)) dynamic ((assert (compare (call "g" ()) ((== (con 42))))) (assert (compare (call "g" ()) ((== (con 42))))))) (expr (call "main" ("g")))))))))))

;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f0" () dynamic ((return (con 42)))) (function-def "f1" (("a" dynamic) ("b" dynamic) ("c" dynamic) ("d" dynamic) ("e" dynamic) ("f" dynamic) ("g" dynamic) ("h" dynamic)) dynamic ((class "C" () (pass)) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (call "f0" ()) "a") "b") "c") "d") "e") "f") "g") "h") (con 4))))) (function-def "f2" () dynamic ((return (call "f1" ((con 1) (con 2) (con 3) (con 4) (con 5) (con 6) (con 7) (con 8)))))) (function-def "f3" () dynamic ((return (call "f2" ())))) (function-def "f4" () dynamic ((return (call "f3" ())))) (function-def "f5" () dynamic ((return (call "f4" ())))) (function-def "f6" () dynamic ((return (call "f5" ())))) (function-def "f7" () dynamic ((return (call "f6" ())))) (function-def "f8" () dynamic ((return (call "f7" ())))) (function-def "f9" () dynamic ((return (call "f8" ())))) (function-def "f10" () dynamic ((return (call "f9" ())))) (function-def "f11" () dynamic ((return (call "f10" ())))) (function-def "g" () dynamic ((return (call "f11" ())))) (function-def "main" (("g" dynamic) ("f1" dynamic)) dynamic ((assert (compare (call "g" ()) ((== (con 42))))) (assert (compare (call "g" ()) ((== (con 42))))))) (expr (call "main" ("g" "f1")))))))))))

;; conformance_suite/test_max.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "max" ("a" "b"))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con 1) (con 3))) ((== (con 3))))) (assert (compare (call "f" ((con 3) (con 1))) ((== (con 3))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_method_prologue.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" "str")) dynamic ((return (con 42)))) (function-def "main" (("f" dynamic)) dynamic ((try-except-else-finally ((expr (call "f" ((con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_method_prologue_2.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" dynamic) ("y" "str")) dynamic ((return (con 42)))) (function-def "main" (("f" dynamic)) dynamic ((try-except-else-finally ((expr (call "f" ((con "abc") (con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_method_prologue_3.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" "int") ("y" "str")) dynamic ((return (con 42)))) (function-def "main" (("f" dynamic)) dynamic ((try-except-else-finally ((expr (call "f" ((con 42) (con 42))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_method_prologue_no_annotation.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("x" dynamic)) dynamic ((return (con 42)))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con "abc"))) ((== (con 42))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_min.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("a" "int") ("b" "int")) "int" ((return (call "min" ("a" "b"))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con 1) (con 3))) ((== (con 1))))) (assert (compare (call "f" ((con 3) (con 1))) ((== (con 1))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_multiply_list_exact_by_int.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () "int" ((assign ("l") (bin-op * (list ((con 1) (con 2) (con 3))) (con 2))) (return (call "len" ("l"))))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (con 6))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_multiply_list_exact_by_int_reverse.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () "int" ((assign ("l") (bin-op * (con 2) (list ((con 1) (con 2) (con 3))))) (return (call "len" ("l"))))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (con 6))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_nested_fn_type_error.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("i" "int") ("j" "str") ("l" "int") ("m" "int") ("n" "int") ("o" "int")) "bool" ((function-def "g" (("k" "int")) "bool" ((return (if-exp (compare "j" ((== (con "gt")))) (compare "k" ((> (con 0)))) (compare "k" ((<= (con 0)))))))) (return (call "g" ("i"))))) (function-def "main" (("f" dynamic)) dynamic ((try-except-else-finally ((expr (call "f" ((con 1) (con "a") (con 2) (con 3) (con "4") (con 5))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_nested_fn_type_error_2.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("i" "int") ("j" "str") ("k" "int")) "bool" ((function-def "g" (("k" "int")) "bool" ((return (if-exp (compare "j" ((== (con "gt")))) (compare "k" ((> (con 0)))) (compare "k" ((<= (con 0)))))))) (return (call "g" ("i"))))) (function-def "main" (("f" dynamic)) dynamic ((try-except-else-finally ((expr (call "f" ((con 1) (con 2) (con 3))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_no_narrow_to_dynamic.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((return (con 42)))) (function-def "g" () dynamic ((ann-assign "x" "int" (con 100)) (assign ("x") (call "f" ())) (return (call (attribute "x" "bit_length") ())))) (function-def "main" (("g" dynamic)) dynamic ((assert (compare (call "g" ()) ((== (con 6))))))) (expr (call "main" ("g")))))))))))

;; conformance_suite/test_nonarray_len.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "Lol" () ((function-def "__len__" (("self" dynamic)) dynamic ((return (con 421)))))) (function-def "y" () dynamic ((return (call "len" ((call "Lol" ())))))) (function-def "main" (("y" dynamic)) dynamic ((assert (compare (call "y" ()) ((== (con 421))))))) (expr (call "main" ("y")))))))))))

;; conformance_suite/test_override_bad_ret.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "B" () ((function-def "f" (("self" dynamic)) (con "B") ((return "self"))))) (function-def "f" (("x" "B")) dynamic ((return (call (attribute "x" "f") ())))) (function-def "main" (("B" dynamic) ("f" dynamic)) dynamic ((class "D" ("B") ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))))) (try-except-else-finally ((expr (call "f" ((call "D" ()))))) ((except-handler "TypeError" None (pass))) ((raise (call "Exception" ()))) ()))) (expr (call "main" ("B" "f")))))))))))

;; conformance_suite/test_package_no_parent.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" () ((function-def "f" (("self" dynamic)) dynamic ((return (con 42)))))) (function-def "main" (("C" dynamic)) dynamic ((assert (compare (call (attribute (call "C" ()) "f") ()) ((== (con 42))))))) (expr (call "main" ("C")))))))))))

;; conformance_suite/test_pydict_arg_annotation.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (function-def "f" (("d" (subscript "PyDict" (tuple ("str" "int"))))) "str" ((return (subscript "d" (con 3))))) (function-def "main" () dynamic ((assert (compare (call "f" ((dict (((con 3) (con "foo")))))) ((== (con "foo"))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_refine_or_expression.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" (("s" (subscript "Optional" "str"))) "str" ((return (bool-op or ("s" (con "hi")))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con "A"))) ((== (con "A"))))) (assert (compare (call "f" ((con None))) ((== (con "hi"))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_refine_or_expression_with_multiple_optionals.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Optional")) (function-def "f" (("s1" (subscript "Optional" "str")) ("s2" (subscript "Optional" "str"))) "str" ((return (bool-op or ("s1" "s2" (con "hi")))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con "A") (con None))) ((== (con "A"))))) (assert (compare (call "f" ((con None) (con "B"))) ((== (con "B"))))) (assert (compare (call "f" ((con "A") (con "B"))) ((== (con "A"))))) (assert (compare (call "f" ((con None) (con None))) ((== (con "hi"))))))) (expr (call "main" ("f")))))))))))

;; conformance_suite/test_ret_type_cast.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (function-def "testfunc" (("x" "str") ("y" "str")) "bool" ((return (compare "x" ((== "y")))))) (function-def "main" (("f" dynamic)) dynamic ((assert (compare (call "f" ((con "abc") (con "abc"))) ((== (con #t))))))) (expr (call "main" ("testfunc")))))))))))

;; conformance_suite/test_seq_repeat_inexact_list.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("List")) (function-def "f" (("l" (subscript "List" "int"))) dynamic ((return (bin-op * "l" (con 2))))) (function-def "main" () dynamic ((assert (compare (call "f" ((list ((con 1) (con 2))))) ((== (list ((con 1) (con 2) (con 1) (con 2))))))) (class "MyList" ("list") ((function-def "__mul__" (("self" dynamic) ("other" dynamic)) dynamic ((return (con "RESULT")))))) (assert (compare (call "f" ((call "MyList" ((list ((con 1) (con 2))))))) ((== (con "RESULT"))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_seq_repeat_inexact_num.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" (("num" "int")) dynamic ((return (bin-op * "num" (list ((con 1) (con 2))))))) (function-def "main" () dynamic ((assert (compare (call "f" ((con 2))) ((== (list ((con 1) (con 2) (con 1) (con 2))))))) (class "MyInt" ("int") ((function-def "__mul__" (("self" dynamic) ("other" dynamic)) dynamic ((return (con "RESULT")))))) (assert (compare (call "f" ((call "MyInt" ((con 2))))) ((== (con "RESULT"))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_seq_repeat_inexact_tuple.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Tuple")) (function-def "f" (("t" (subscript "Tuple" "int"))) dynamic ((return (bin-op * "t" (con 2))))) (assert (compare (call "f" ((tuple ((con 1) (con 2))))) ((== (tuple ((con 1) (con 2) (con 1) (con 2))))))) (class "MyTuple" ("tuple") ((function-def "__mul__" (("self" dynamic) ("other" dynamic)) dynamic ((return (con "RESULT")))))) (assert (compare (call "f" ((call "MyTuple" ((tuple ((con 1) (con 2))))))) ((== (con "RESULT")))))))))))))

;; conformance_suite/test_seq_repeat_list.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("l") (list ((con 1) (con 2)))) (return (bin-op * "l" (con 2))))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (list ((con 1) (con 2) (con 1) (con 2))))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_seq_repeat_list_reversed.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("l") (list ((con 1) (con 2)))) (return (bin-op * (con 2) "l")))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (list ((con 1) (con 2) (con 1) (con 2))))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_seq_repeat_tuple.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("t") (tuple ((con 1) (con 2)))) (return (bin-op * "t" (con 2))))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (tuple ((con 1) (con 2) (con 1) (con 2))))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_seq_repeat_tuple_reversed.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((assign ("t") (tuple ((con 1) (con 2)))) (return (bin-op * (con 2) "t")))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (tuple ((con 1) (con 2) (con 1) (con 2))))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_str_split.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "get_str" () "str" ((return (con "something here")))) (function-def "test" () "str" ((assign ((tuple ("a" "b"))) (call (attribute (call "get_str" ()) "split") ((con None) (con 1)))) (return "b"))) (function-def "main" (("test" dynamic)) dynamic ((assert (compare (call "test" ()) ((== (con "here"))))))) (expr (call "main" ("test")))))))))))

;; conformance_suite/test_try_return_finally.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("List")) (function-def "f1" (("x" "List")) dynamic ((try-except-else-finally ((return (con None))) () () ((expr (call (attribute "x" "append") ((con "hi")))))))) (function-def "main" (("f1" dynamic)) dynamic ((assign ("l") (list ())) (expr (call "f1" ("l"))) (assert (compare "l" ((== (list ((con "hi"))))))))) (expr (call "main" ("f1")))))))))))

;; conformance_suite/test_verify_lambda.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((assign ("x") (lambda (("x" dynamic)) "x")) (assign ("a") (call "x" ((con "hi")))) (function-def "main" () dynamic ((assert (compare "a" ((== (con "hi"))))))) (expr (call "main" ()))))))))))

;; conformance_suite/test_visit_if_else.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((assign ("x") (con 0)) (if "x" (pass) ((function-def "f" () dynamic ((return (con 42)))))) (function-def "main" () dynamic ((assert (compare (call "f" ()) ((== (con 42))))))) (expr (call "main" ()))))))))))

;; conformance_suite/try_except_basic.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((try-except-else-finally ((ann-assign "x" "int" (con 42))) ((except-handler "Exception" None (pass))) (pass) (pass)) (assert (compare "x" ((is (con 42)))))))))))))

;; conformance_suite/try_except_catch_else_no_exn.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((try-except-else-finally (pass) ((except-handler "Exception" None ((return (con 2))))) ((return (con 3))) ()))) (assert (compare (call "f" ()) ((is (con 3)))))))))))))

;; conformance_suite/try_except_catch_else_some_exn.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((try-except-else-finally ((raise (call "Exception" ((con "foo"))))) ((except-handler "Exception" None ((return (con 2))))) ((return (con 3))) ()))) (assert (compare (call "f" ()) ((is (con 2)))))))))))))

;; conformance_suite/try_except_catch_final_no_exn.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((try-except-else-finally (pass) ((except-handler "Exception" None ((return (con 2))))) ((return (con 3))) ((return (con 42)))))) (assert (compare (call "f" ()) ((is (con 42)))))))))))))

;; conformance_suite/try_except_catch_final_some_exn.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((try-except-else-finally ((raise (call "Exception" ((con "foo"))))) ((except-handler "Exception" None ((return (con 2))))) ((return (con 3))) ((return (con 42)))))) (assert (compare (call "f" ()) ((is (con 42)))))))))))))

;; conformance_suite/try_except_catch_same_class.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((try-except-else-finally ((raise (call "Exception" ((con "foo"))))) ((except-handler "Exception" None ((return (con 42))))) () ()))) (assert (compare (call "f" ()) ((is (con 42)))))))))))))

;; conformance_suite/try_except_catch_sub_class.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C" ("Exception") (pass)) (function-def "f" () dynamic ((try-except-else-finally ((raise (call "C" ((con "foo"))))) ((except-handler "Exception" None ((return (con 42))))) () ()))) (assert (compare (call "f" ()) ((is (con 42)))))))))))))

;; conformance_suite/tuple_is_inhabitable.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "tuple" (tuple ()))))))))))

;; conformance_suite/union_optional_is_supported_pos.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Union")) (function-def "f" (("x" (subscript "Union" (tuple ((con None) "int"))))) dynamic (pass)) (expr (call "f" ((con None)))) (expr (call "f" ((con 42))))))))))))

;; conformance_suite/union_other_is_dyn.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((import-from "typing" ("Union")) (function-def "f" (("x" (subscript "Union" (tuple ("str" "int"))))) dynamic (pass)) (class "C" () (pass)) (expr (call "f" ((call "C" ()))))))))))))

;; conformance_suite/upcast_C1_to_C2.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((class "C1" () (pass)) (class "C2" ("C1") (pass)) (function-def "f" () dynamic ((return (call "C2" ())))) (ann-assign "x" "C1" (call "f" ()))))))))))

;; conformance_suite/upcast_bool_to_int.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((ann-assign "x" "bool" (con #t)) (ann-assign "y" "int" "x")))))))))

;; conformance_suite/while-loop_basic.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "fact" (("i" "int")) "int" ((ann-assign "o" "int" (con 1)) (while (compare "i" ((is-not (con 0)))) ((aug-assign "o" * "i") (aug-assign "i" - (con 1))) ()) (return "o"))) (assert (compare (call "fact" ((con 5))) ((is (con 120)))))))))))))

;; conformance_suite/while-loop_else.py
(check-not-exn (lambda () (test-match SP-dynamics (terminate) (term (calc (compile-program (desugar-program ((function-def "f" () dynamic ((while (compare (con "orange") ((is (con "apple")))) ((return (con 2))) ((return (con 3)))) (assert (con #f)))) (assert (compare (call "f" ()) ((is (con 3)))))))))))))
