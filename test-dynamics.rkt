#lang racket
(require redex)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")
(require "dynamics.rkt")

;; conformance_suite/CheckedDict_delete_bad_key.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "other"))))))))

;; conformance_suite/CheckedDict_delete_checks_keys.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def asDyn ((x dynamic)) dynamic (begin (return x))) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (delete (subscript (call asDyn x) 42))))))))

;; conformance_suite/CheckedDict_delete_good_key.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "bar"))))))))

;; conformance_suite/CheckedDict_delete_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x "foo"))))))))

;; conformance_suite/CheckedDict_delete_then_lookup.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "bar")) (expr (subscript x "bar"))))))))

;; conformance_suite/CheckedDict_from_bad_dict.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax int str)) (call (subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 4))))))))))

;; conformance_suite/CheckedDict_from_good_dict.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax int str)) (call (subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 "b"))))))))))

;; conformance_suite/CheckedDict_from_nondict.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax int str)) (call (subscript CheckedDict (tuple-syntax int str)) 42))))))))

;; conformance_suite/CheckedDict_insert.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (assign (subscript x "new") dynamic 4)))))))

;; conformance_suite/CheckedDict_insert_then_lookup.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax))) (assign (subscript x "foo") dynamic 42) (expr (subscript x "foo"))))))))

;; conformance_suite/CheckedDict_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3))))))))))

;; conformance_suite/CheckedDict_key_can_be_Optional.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (import-from "typing" (Optional)) (ann-assign x (subscript CheckedDict (tuple-syntax (subscript Optional str) int)) (call (subscript CheckedDict (tuple-syntax (subscript Optional str) int)) (dict-syntax ("foo" 2) (None 3)))) (assert (compare (subscript x None) ((is 3))))))))))

;; conformance_suite/CheckedDict_lookup_checks_keys.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def asDyn ((x dynamic)) dynamic (begin (return x))) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (expr (subscript (call asDyn x) 42))))))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (expr (subscript x "foo"))))))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (ann-assign y int (subscript x "foo"))))))))

;; conformance_suite/CheckedDict_update.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (assign (subscript x "bar") dynamic 4)))))))

;; conformance_suite/CheckedDict_update_checks_keys.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def asDyn ((x dynamic)) dynamic (begin (return x))) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (assign (subscript (call asDyn x) 42) dynamic "bar")))))))

;; conformance_suite/CheckedDict_update_checks_values.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def asDyn ((x dynamic)) dynamic (begin (return x))) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (assign (subscript (call asDyn x) "foo") dynamic "bar")))))))

;; conformance_suite/CheckedDict_update_key_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (assign (subscript x "bar") dynamic 3)))))))

;; conformance_suite/CheckedDict_update_then_lookup.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (assign (subscript x "foo") dynamic 3) (assert (compare (subscript x "foo") ((is 3))))))))))

;; conformance_suite/CheckedDict_update_val_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict CheckedDict)) (ann-assign x (subscript CheckedDict (tuple-syntax str int)) (call (subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (assign (subscript x "bar") dynamic 2)))))))

;; conformance_suite/CheckedDict_val_can_be_Optional.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (import-from "typing" (Optional)) (ann-assign x (subscript CheckedDict (tuple-syntax str (subscript Optional int))) (call (subscript CheckedDict (tuple-syntax str (subscript Optional int))) (dict-syntax ("foo" 2) ("bar" None)))) (assert (compare (subscript x "bar") ((is None))))))))))

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "other"))))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar"))))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")) (expr (subscript x "bar"))))))))

;; conformance_suite/PyDict_insert.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (assign (subscript x "new") dynamic "hello")))))))

;; conformance_suite/PyDict_insert_then_lookup.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax)) (assign (subscript x "foo") dynamic 42) (expr (subscript x "foo"))))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2)))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "other"))))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar"))))))))

;; conformance_suite/PyDict_update.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (assign (subscript x "bar") dynamic "hello")))))))

;; conformance_suite/PyDict_update_then_lookup.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (ann-assign x PyDict (dict-syntax ("foo" 2))) (assign (subscript x "foo") dynamic 3) (assert (compare (subscript x "foo") ((is 3))))))))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((ann-assign x bool #t)))))))

;; conformance_suite/classes_are_not_first-class.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class C () pass) (function-def checkExpect ((cls dynamic) (obj dynamic)) dynamic (begin (ann-assign x cls obj) (return x))) (expr (call checkExpect C 42))))))))

;; conformance_suite/downcast_float_to_int_neg.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((function-def asDyn ((x dynamic)) dynamic (begin (return x))) (ann-assign x float 2.3) (ann-assign y int (call asDyn x))))))))

;; conformance_suite/downcast_float_to_int_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def asDyn ((x dynamic)) dynamic (begin (return x))) (ann-assign x float 2) (ann-assign y int (call asDyn x))))))))

;; conformance_suite/empty_program.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ())))))

;; conformance_suite/float_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((ann-assign x float 2.3)))))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((ann-assign x int 42)))))))

;; conformance_suite/procedure_check_argument_type_dynamically.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((function-def asDyn ((x dynamic)) dynamic (begin (return x))) (function-def f ((x int)) dynamic (begin pass)) (expr (call f (call asDyn "foo")))))))))

;; conformance_suite/procedure_check_arity_dynamically.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((function-def asDyn ((x dynamic)) dynamic (begin (return x))) (function-def f ((x dynamic) (y dynamic)) dynamic (begin pass)) (expr (call (call asDyn f) 2))))))))

;; conformance_suite/procedure_check_return_type_dynamically.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((function-def asDyn ((x dynamic)) dynamic (begin (return x))) (function-def f () str (begin (return (call asDyn 2)))) (expr (call f))))))))

;; conformance_suite/procedure_works.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((x int) (y dynamic)) dynamic (begin (return (bin-op - y x)))) (assert (compare (call f 2 3) ((is 1))))))))))

;; conformance_suite/test_assert_narrowing_debug.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def foo ((x (bin-op bit-or int str))) int (begin (assert (call isinstance x int)) (return (bin-op + x 1))))))))))

;; conformance_suite/test_assert_narrowing_not_isinstance_optimized.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def foo ((x (bin-op bit-or int str))) str (begin (assert (unary-op not (call isinstance x int))) (return x)))))))))

;; conformance_suite/test_assert_narrowing_optimized.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def foo ((x (bin-op bit-or int str))) object (begin (assert (call isinstance x int)) (return x)))))))))

;; conformance_suite/test_aug_assign.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((l dynamic)) dynamic (begin (aug-assign (subscript l 0) + 1)))))))))

;; conformance_suite/test_augassign_inexact.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def something () dynamic (begin (return 3))) (function-def t () dynamic (begin (ann-assign a int (call something)) (assign b dynamic 0) (aug-assign b + a) (return b)))))))))

;; conformance_suite/test_bind_boolop_type.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def f ((self dynamic)) bool (begin (return #t))) (function-def g ((self dynamic)) bool (begin (return #f))) (function-def x ((self dynamic)) bool (begin (return (bool-op and (call (attribute self "f")) (call (attribute self "g")))))) (function-def y ((self dynamic)) bool (begin (return (bool-op or (call (attribute self "f")) (call (attribute self "g")))))))))))))

;; conformance_suite/test_bind_none_compare_op.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (function-def has--none ((x dynamic)) bool (begin (return (compare None ((in x)))))) (function-def has--no--none ((x dynamic)) bool (begin (return (compare None ((not-in x))))))))))))

;; conformance_suite/test_call_function_unknown_ret_type.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__future__" (annotations)) (function-def g () foo (begin (return 42))) (function-def testfunc () dynamic (begin (return (call g))))))))))

;; conformance_suite/test_chkdict_literal.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def testfunc () dynamic (begin (ann-assign x (subscript CheckedDict (tuple-syntax int str)) (dict-syntax)) (return x)))))))))

;; conformance_suite/test_class_static_tpflag.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class A () pass)))))))

;; conformance_suite/test_class_unknown_decorator.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def dec ((f dynamic)) dynamic (begin (return f))) (class C () (function-def foo ((self dynamic)) int (begin (return 3))) (function-def f ((self dynamic)) dynamic (begin (return (call (attribute self "foo"))))))))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (pydict)) (class B () pass) (class D (B) pass) (function-def testfunc () dynamic (begin (ann-assign x pydict (dict-syntax ((call B) 42) ((call D) 42))) (return x)))))))))

;; conformance_suite/test_compile_checked_dict_explicit_dict_as_dict.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (pydict)) (class B () pass) (class D (B) pass) (function-def testfunc () dynamic (begin (ann-assign x dict (dict-syntax ((call B) 42) ((call D) 42))) (return x)))))))))

;; conformance_suite/test_compile_checked_dict_len.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def testfunc () dynamic (begin (assign x dynamic (call (subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (return (call len x))))))))))

;; conformance_suite/test_compile_checked_dict_opt_out_by_default.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class B () pass) (class D (B) pass) (function-def testfunc () dynamic (begin (assign x dynamic (dict-syntax ((call B) 42) ((call D) 42))) (return x)))))))))

;; conformance_suite/test_compile_checked_dict_optional.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (import-from "typing" (Optional)) (function-def testfunc () dynamic (begin (assign x dynamic (call (subscript CheckedDict (tuple-syntax str (bin-op bit-or str None))) (dict-syntax ("x" None) ("y" "z")))) (return x)))))))))

;; conformance_suite/test_compile_checked_dict_reversed.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (class B () pass) (class D (B) pass) (function-def testfunc () dynamic (begin (assign x dynamic (call (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((call D) 42) ((call B) 42)))) (return x)))))))))

;; conformance_suite/test_compile_checked_dict_type_specified.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (class B () pass) (class D (B) pass) (function-def testfunc () dynamic (begin (ann-assign x (subscript CheckedDict (tuple-syntax B int)) (call (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((call D) 42)))) (return x)))))))))

;; conformance_suite/test_compile_checked_dict_with_annotation.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (class B () pass) (function-def testfunc () dynamic (begin (ann-assign x (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((call B) 42))) (return x)))))))))

;; conformance_suite/test_compile_dict_setitem.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def testfunc () dynamic (begin (assign x dynamic (call (subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (expr (call (attribute x "__setitem__") 2 "def")) (return x)))))))))

;; conformance_suite/test_compile_dict_setitem_subscr.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def testfunc () dynamic (begin (assign x dynamic (call (subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (assign (subscript x 2) dynamic "def") (return x)))))))))

;; conformance_suite/test_compile_nested_class_in_fn.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def fn () dynamic (begin (class C () (ann-assign c int 1)) (return (call C))))))))))

;; conformance_suite/test_compile_nested_dict.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (class B () pass) (class D (B) pass) (function-def testfunc () dynamic (begin (assign x dynamic (call (subscript CheckedDict (tuple-syntax B int)) (dict-syntax ((call B) 42) ((call D) 42)))) (assign y dynamic (call (subscript CheckedDict (tuple-syntax int (subscript CheckedDict (tuple-syntax B int)))) (dict-syntax (42 x)))) (return y)))))))))

;; conformance_suite/test_decorated_function_ignored.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class C () pass) (function-def mydecorator ((x dynamic)) dynamic (begin (return C))) (function-def f () dynamic (begin (return 42))) (function-def g () dynamic (begin (return (call f))))))))))

;; conformance_suite/test_dict_invoke.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (pydict)) (function-def f ((x dynamic)) dynamic (begin (ann-assign y pydict x) (return (call (attribute y "get") "foo"))))))))))

;; conformance_suite/test_dict_invoke_ret.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (pydict)) (function-def g () dynamic (begin (return None))) (function-def f ((x dynamic)) dynamic (begin (ann-assign y pydict x) (assign z dynamic (call (attribute y "get") "foo")) (assign z dynamic None) (return z)))))))))

;; conformance_suite/test_final_constant_folding_disabled_on_nonfinals.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Final)) (ann-assign X str "omg") (function-def f () str (begin (return (subscript X 1))))))))))

;; conformance_suite/test_generic_method_ret_type.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (import-from "typing" (Optional)) (ann-assign MAP (subscript CheckedDict (tuple-syntax str (subscript Optional str))) (call (subscript CheckedDict (tuple-syntax str (subscript Optional str))) (dict-syntax ("abc" "foo") ("bar" None)))) (function-def f ((x str)) (subscript Optional str) (begin (return (call (attribute MAP "get") x))))))))))

;; conformance_suite/test_incompat_override_init_okay.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class A () (function-def ----init---- ((self dynamic)) None (begin pass))) (class B (A) (function-def ----init---- ((self dynamic) (x int)) None (begin pass))) (function-def f ((x A)) dynamic (begin (expr (call (attribute x "__init__")))))))))))

;; conformance_suite/test_inline_nested.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (inline)) (function-def e ((x dynamic) (y dynamic)) dynamic (begin (return (bin-op + x y)))) (function-def f ((x dynamic) (y dynamic)) dynamic (begin (return (call e x 3)))) (function-def g () dynamic (begin (return (call f 1 2))))))))))

;; conformance_suite/test_inline_nested_arg.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (inline)) (function-def e ((x dynamic) (y dynamic)) dynamic (begin (return (bin-op + x y)))) (function-def f ((x dynamic) (y dynamic)) dynamic (begin (return (call e x 3)))) (function-def g ((a dynamic) (b dynamic)) dynamic (begin (return (call f a b))))))))))

;; conformance_suite/test_inline_recursive.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (inline)) (function-def f ((x dynamic) (y dynamic)) dynamic (begin (return (call f x y)))) (function-def g () dynamic (begin (return (call f 1 2))))))))))

;; conformance_suite/test_invoke_all_extra_args.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7)) g)))) (function-def testfunc () dynamic (begin (return (call target 1 2 3 4 5 6 7))))))))))

;; conformance_suite/test_invoke_all_reg_args.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7))))) (function-def testfunc () dynamic (begin (return (call target 1 2 3 4 5 6))))))))))

;; conformance_suite/test_invoke_chkdict_method.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (CheckedDict)) (function-def dict--maker () (subscript CheckedDict (tuple-syntax int int)) (begin (return (call (subscript CheckedDict (tuple-syntax int int)) (dict-syntax (2 2)))))) (function-def func () dynamic (begin (assign a dynamic (call dict--maker)) (return (call (attribute a "keys")))))))))))

;; conformance_suite/test_invoke_int_method.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def func () dynamic (begin (assign a dynamic 42) (return (call (attribute a "bit_length")))))))))))

;; conformance_suite/test_invoke_method_non_static_base.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class C (Exception) (function-def f ((self dynamic)) dynamic (begin (return 42))) (function-def g ((self dynamic)) dynamic (begin (return (call (attribute self "f"))))))))))))

;; conformance_suite/test_invoke_str_method_arg.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def func () dynamic (begin (assign a dynamic "a b c") (return (call (attribute a "split") "a"))))))))))

;; conformance_suite/test_invoke_strict_module_deep.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f0 () dynamic (begin (return 42))) (function-def f1 () dynamic (begin (return (call f0)))) (function-def f2 () dynamic (begin (return (call f1)))) (function-def f3 () dynamic (begin (return (call f2)))) (function-def f4 () dynamic (begin (return (call f3)))) (function-def f5 () dynamic (begin (return (call f4)))) (function-def f6 () dynamic (begin (return (call f5)))) (function-def f7 () dynamic (begin (return (call f6)))) (function-def f8 () dynamic (begin (return (call f7)))) (function-def f9 () dynamic (begin (return (call f8)))) (function-def f10 () dynamic (begin (return (call f9)))) (function-def f11 () dynamic (begin (return (call f10)))) (function-def g () dynamic (begin (return (call f11))))))))))

;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f0 () dynamic (begin (return 42))) (function-def f1 ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic) (h dynamic)) dynamic (begin (class C () pass) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (call f0) a) b) c) d) e) f) g) h) 4)))) (function-def f2 () dynamic (begin (return (call f1 1 2 3 4 5 6 7 8)))) (function-def f3 () dynamic (begin (return (call f2)))) (function-def f4 () dynamic (begin (return (call f3)))) (function-def f5 () dynamic (begin (return (call f4)))) (function-def f6 () dynamic (begin (return (call f5)))) (function-def f7 () dynamic (begin (return (call f6)))) (function-def f8 () dynamic (begin (return (call f7)))) (function-def f9 () dynamic (begin (return (call f8)))) (function-def f10 () dynamic (begin (return (call f9)))) (function-def f11 () dynamic (begin (return (call f10)))) (function-def g () dynamic (begin (return (call f11))))))))))

;; conformance_suite/test_invoke_strict_module_pre_invoked.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f () dynamic (begin (return 42))) (function-def g () dynamic (begin (return (call f))))))))))

;; conformance_suite/test_max.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((a int) (b int)) int (begin (return (call max a b))))))))))

;; conformance_suite/test_max_stability.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((a int) (b int)) int (begin (return (call max a b))))))))))

;; conformance_suite/test_method_prologue_no_annotation.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((x dynamic)) dynamic (begin (return 42)))))))))

;; conformance_suite/test_min.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((a int) (b int)) int (begin (return (call min a b))))))))))

;; conformance_suite/test_min_stability.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((a int) (b int)) int (begin (return (call min a b))))))))))

;; conformance_suite/test_module_subclass.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class C () (function-def ----init---- ((self dynamic)) dynamic (begin (ann-assign (attribute self "x") (subscript Optional C) None))))))))))

;; conformance_suite/test_named_tuple.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (NamedTuple)) (class C (NamedTuple) (ann-assign x int) (ann-assign y str)) (function-def myfunc ((x C)) dynamic (begin (return (attribute x "x"))))))))))

;; conformance_suite/test_no_narrow_to_dynamic.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f () dynamic (begin (return 42))) (function-def g () dynamic (begin (ann-assign x int 100) (assign x dynamic (call f)) (return (call (attribute x "bit_length")))))))))))

;; conformance_suite/test_none_not.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def t () bool (begin (assign x dynamic None) (if (unary-op not x) ((return #t)) ((return #f)))))))))))

;; conformance_suite/test_override_bad_ret.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class B () (function-def f ((self dynamic)) "B" (begin (return self)))) (function-def f ((x B)) dynamic (begin (return (call (attribute x "f")))))))))))

;; conformance_suite/test_override_okay.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class B () (function-def f ((self dynamic)) "B" (begin (return self)))) (function-def f ((x B)) dynamic (begin (return (call (attribute x "f")))))))))))

;; conformance_suite/test_override_override_inherited.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Optional)) (class B () (function-def f ((self dynamic)) "Optional[B]" (begin (return self)))) (class D (B) pass) (function-def f ((x B)) dynamic (begin (return (call (attribute x "f")))))))))))

;; conformance_suite/test_package_no_parent.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class C () (function-def f ((self dynamic)) dynamic (begin (return 42))))))))))

;; conformance_suite/test_prod_assert.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Optional)) (import-from "__static__" (prod--assert)) (function-def foo ((x (subscript Optional int))) int (begin (expr (call prod--assert x)) (return x)))))))))

;; conformance_suite/test_protocol_is_dynamic.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Protocol)) (class CallableProtocol (Protocol) (function-def ----call---- ((self dynamic) (x int)) str (begin pass))) (function-def foo ((x str)) int (begin (return (call int x)))) (ann-assign c CallableProtocol foo)))))))

;; conformance_suite/test_pydict_arg_annotation.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" (PyDict)) (function-def f ((d (subscript PyDict (tuple-syntax str int)))) str (begin (return (subscript d 3))))))))))

;; conformance_suite/test_refine_or_expression.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Optional)) (function-def f ((s (subscript Optional str))) str (begin (return (bool-op or s "hi"))))))))))

;; conformance_suite/test_refine_or_expression_with_multiple_optionals.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Optional)) (function-def f ((s1 (subscript Optional str)) (s2 (subscript Optional str))) str (begin (return (bool-op or s1 s2 "hi"))))))))))

;; conformance_suite/test_ret_type_cast.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (function-def testfunc ((x str) (y str)) bool (begin (return (compare x ((== y))))))))))))

;; conformance_suite/test_slotification_decorated.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((class --Inner () pass) (function-def something ((klass dynamic)) dynamic (begin (return --Inner))) (class C () (function-def f ((self dynamic)) dynamic (begin pass))) (function-def f () dynamic (begin (return (call (attribute (call C) "f")))))))))))

;; conformance_suite/test_str_split.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def get--str () str (begin (return "something here"))) (function-def test () str (begin (assign (tuple-syntax a b) dynamic (call (attribute (call get--str) "split") None 1)) (return b)))))))))

;; conformance_suite/test_union_compare.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((function-def f ((x (bin-op bit-or int float))) bool (begin (return (compare x ((> 0))))))))))))

;; conformance_suite/test_unknown_isinstance_bool_ret.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x))) (function-def ----eq---- ((self dynamic) (other Any)) bool (begin (return (call isinstance other C)))))))))))

;; conformance_suite/test_unknown_isinstance_narrows.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x)))) (function-def testfunc ((x dynamic)) dynamic (begin (if (call isinstance x C) ((return (attribute x "x"))) ())))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x))) (function-def f ((self dynamic) (other dynamic)) str (begin (if (call isinstance other (attribute self "__class__")) ((return (attribute other "x"))) ()) (return ""))))))))))

;; conformance_suite/test_unknown_isinstance_narrows_class_attr_dynamic.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x))) (function-def f ((self dynamic) (other dynamic) (unknown dynamic)) dynamic (begin (if (call isinstance other (attribute unknown "__class__")) ((return (attribute other "x"))) ()) (return ""))))))))))

;; conformance_suite/test_unknown_isinstance_narrows_else_correct.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x)))) (function-def testfunc ((x dynamic)) dynamic (begin (if (call isinstance x C) (pass) ((return (attribute x "x"))))))))))))

;; conformance_suite/test_unknown_issubclass_bool_ret.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x))) (function-def ----eq---- ((self dynamic) (other Any)) bool (begin (return (call issubclass (call type other) C)))))))))))

;; conformance_suite/test_unknown_param_ann.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" (Any)) (class C () (function-def ----init---- ((self dynamic) (x str)) dynamic (begin (ann-assign (attribute self "x") str x))) (function-def ----eq---- ((self dynamic) (other Any)) bool (begin (return #f))))))))))

;; conformance_suite/test_verify_lambda.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((assign x dynamic (lambda ((x dynamic)) x)) (assign a dynamic (call x "hi"))))))))

;; conformance_suite/test_visit_if_else.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((assign x dynamic 0) (if x (pass) ((function-def f () dynamic (begin (return 42)))))))))))

;; conformance_suite/upcast_bool_to_float.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((ann-assign x bool #t) (ann-assign y float x)))))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((ann-assign x bool #t) (ann-assign y int x)))))))

;; conformance_suite/upcast_int_to_float.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((ann-assign x int 2) (ann-assign y float x)))))))
