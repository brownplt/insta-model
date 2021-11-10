#lang racket
(require redex)
(require "desugar.rkt")
(require "compile.rkt")

;; conformance_suite/CheckedDict_delete_bad_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "other")))))))

;; conformance_suite/CheckedDict_delete_checks_keys.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (delete (subscript (asDyn x) 42)))))))

;; conformance_suite/CheckedDict_delete_good_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "bar")))))))

;; conformance_suite/CheckedDict_delete_pos.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x "foo")))))))

;; conformance_suite/CheckedDict_delete_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (delete (subscript x "bar")) (expr (subscript x "bar")))))))

;; conformance_suite/CheckedDict_from_bad_dict.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 4)))))))))

;; conformance_suite/CheckedDict_from_good_dict.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 "b")))))))))

;; conformance_suite/CheckedDict_from_nondict.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) 42)))))))

;; conformance_suite/CheckedDict_insert.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (define/assign (subscript x "new") dynamic 4))))))

;; conformance_suite/CheckedDict_insert_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax))) (define/assign (subscript x "foo") dynamic 42) (expr (subscript x "foo")))))))

;; conformance_suite/CheckedDict_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))))))))

;; conformance_suite/CheckedDict_key_can_be_Optional.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign x (subscript CheckedDict (tuple-syntax (subscript Optional str) int)) ((subscript CheckedDict (tuple-syntax (subscript Optional str) int)) (dict-syntax ("foo" 2) (None 3)))) (assert (is (subscript x None) 3)))))))

;; conformance_suite/CheckedDict_lookup_checks_keys.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (expr (subscript (asDyn x) 42)))))))

;; conformance_suite/CheckedDict_lookup_key_pos.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (expr (subscript x "foo")))))))

;; conformance_suite/CheckedDict_lookup_val_pos.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y int (subscript x "foo")))))))

;; conformance_suite/CheckedDict_update.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2) ("bar" 3)))) (define/assign (subscript x "bar") dynamic 4))))))

;; conformance_suite/CheckedDict_update_checks_keys.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign (subscript (asDyn x) 42) dynamic "bar"))))))

;; conformance_suite/CheckedDict_update_checks_values.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign (subscript (asDyn x) "foo") dynamic "bar"))))))

;; conformance_suite/CheckedDict_update_key_pos.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic 3))))))

;; conformance_suite/CheckedDict_update_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 2)))) (define/assign (subscript x "foo") dynamic 3) (assert (is (subscript x "foo") 3)))))))

;; conformance_suite/CheckedDict_update_val_pos.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic 2))))))

;; conformance_suite/CheckedDict_val_can_be_Optional.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign x (subscript CheckedDict (tuple-syntax str (subscript Optional int))) ((subscript CheckedDict (tuple-syntax str (subscript Optional int))) (dict-syntax ("foo" 2) ("bar" None)))) (assert (is (subscript x "bar") None)))))))

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "other")))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")) (expr (subscript x "bar")))))))

;; conformance_suite/PyDict_insert.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") dynamic "hello"))))))

;; conformance_suite/PyDict_insert_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax)) (define/assign (subscript x "foo") dynamic 42) (expr (subscript x "foo")))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "other")))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar")))))))

;; conformance_suite/PyDict_update.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") dynamic "hello"))))))

;; conformance_suite/PyDict_update_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax ("foo" 2))) (define/assign (subscript x "foo") dynamic 3) (assert (is (subscript x "foo") 3)))))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x bool #t))))))

;; conformance_suite/downcast_float_to_int_neg.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x float 2.3) (define/assign y int (asDyn x)))))))

;; conformance_suite/downcast_float_to_int_pos.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (define/assign x float 2) (define/assign y int (asDyn x)))))))

;; conformance_suite/empty_program.py
(test-match SP-compiled program- (term (compile-program (desugar-program ()))))

;; conformance_suite/float_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x float 2.3))))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x int 42))))))

;; conformance_suite/procedure_check_argument_type_dynamically.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (def f ((x int)) dynamic (begin pass)) (expr (f (asDyn "foo"))))))))

;; conformance_suite/procedure_check_arity_dynamically.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (def f ((x dynamic) (y dynamic)) dynamic (begin pass)) (expr ((asDyn f) 2)))))))

;; conformance_suite/procedure_check_return_type_dynamically.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def asDyn ((x dynamic)) dynamic (begin (return x))) (def f () str (begin (return (asDyn 2)))) (expr (f)))))))

;; conformance_suite/procedure_works.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def f ((x int) (y dynamic)) dynamic (begin (return (bin-op - y x)))) (assert (is (f 2 3) 1)))))))

;; conformance_suite/test_compile_dict_setitem.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic (begin (define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (1 "abc")))) (expr ((attribute x "__setitem__") 2 "def")) (return x))))))))

;; conformance_suite/test_generic_method_ret_type.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (import-from "typing" ("Optional")) (define/assign MAP (subscript CheckedDict (tuple-syntax str (subscript Optional str))) ((subscript CheckedDict (tuple-syntax str (subscript Optional str))) (dict-syntax ("abc" "foo") ("bar" None)))) (def f ((x str)) (subscript Optional str) (begin (return ((attribute MAP "get") x)))))))))

;; conformance_suite/test_inline_recursive.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("inline")) (def f ((x dynamic) (y dynamic)) dynamic (begin (return (f x y)))) (def g () dynamic (begin (return (f 1 2)))))))))

;; conformance_suite/test_invoke_all_extra_args.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7)) g)))) (def testfunc () dynamic (begin (return (target 1 2 3 4 5 6 7)))))))))

;; conformance_suite/test_invoke_all_reg_args.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def target ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic)) dynamic (begin (return (bin-op + (bin-op + (bin-op + (bin-op + (bin-op + (bin-op * a 2) (bin-op * b 3)) (bin-op * c 4)) (bin-op * d 5)) (bin-op * e 6)) (bin-op * f 7))))) (def testfunc () dynamic (begin (return (target 1 2 3 4 5 6)))))))))

;; conformance_suite/test_invoke_chkdict_method.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("CheckedDict")) (def dict--maker () (subscript CheckedDict (tuple-syntax int int)) (begin (return ((subscript CheckedDict (tuple-syntax int int)) (dict-syntax (2 2)))))) (def func () dynamic (begin (define/assign a dynamic (dict--maker)) (return ((attribute a "keys"))))))))))

;; conformance_suite/test_invoke_int_method.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def func () dynamic (begin (define/assign a dynamic 42) (return ((attribute a "bit_length"))))))))))

;; conformance_suite/test_invoke_str_method.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def func () dynamic (begin (define/assign a dynamic "a b c") (return ((attribute a "split"))))))))))

;; conformance_suite/test_invoke_str_method_arg.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def func () dynamic (begin (define/assign a dynamic "a b c") (return ((attribute a "split") "a")))))))))

;; conformance_suite/test_invoke_strict_module_deep.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def f0 () dynamic (begin (return 42))) (def f1 () dynamic (begin (return (f0)))) (def f2 () dynamic (begin (return (f1)))) (def f3 () dynamic (begin (return (f2)))) (def f4 () dynamic (begin (return (f3)))) (def f5 () dynamic (begin (return (f4)))) (def f6 () dynamic (begin (return (f5)))) (def f7 () dynamic (begin (return (f6)))) (def f8 () dynamic (begin (return (f7)))) (def f9 () dynamic (begin (return (f8)))) (def f10 () dynamic (begin (return (f9)))) (def f11 () dynamic (begin (return (f10)))) (def g () dynamic (begin (return (f11)))))))))

;; conformance_suite/test_invoke_strict_module_deep_unjitable_many_args.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def f0 () dynamic (begin (return 42))) (def f1 ((a dynamic) (b dynamic) (c dynamic) (d dynamic) (e dynamic) (f dynamic) (g dynamic) (h dynamic)) dynamic (begin (class C (object)) (return (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (bin-op + (bin-op - (f0) a) b) c) d) e) f) g) h) 4)))) (def f2 () dynamic (begin (return (f1 1 2 3 4 5 6 7 8)))) (def f3 () dynamic (begin (return (f2)))) (def f4 () dynamic (begin (return (f3)))) (def f5 () dynamic (begin (return (f4)))) (def f6 () dynamic (begin (return (f5)))) (def f7 () dynamic (begin (return (f6)))) (def f8 () dynamic (begin (return (f7)))) (def f9 () dynamic (begin (return (f8)))) (def f10 () dynamic (begin (return (f9)))) (def f11 () dynamic (begin (return (f10)))) (def g () dynamic (begin (return (f11)))))))))

;; conformance_suite/test_invoke_strict_module_mutual_recursive.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def fib1 ((number dynamic)) dynamic (begin (if (<= number 1) (begin (return number)) (begin)) (return (bin-op + (fib (bin-op - number 1)) (fib (bin-op - number 2)))))) (def fib ((number dynamic)) dynamic (begin (if (<= number 1) (begin (return number)) (begin)) (return (bin-op + (fib1 (bin-op - number 1)) (fib1 (bin-op - number 2)))))))))))

;; conformance_suite/test_invoke_strict_module_pre_invoked.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def f () dynamic (begin (return 42))) (def g () dynamic (begin (return (f)))))))))

;; conformance_suite/test_invoke_strict_module_recursive.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((def fib ((number dynamic)) dynamic (begin (if (<= number 1) (begin (return number)) (begin)) (return (bin-op + (fib (bin-op - number 1)) (fib (bin-op - number 2)))))))))))

;; conformance_suite/upcast_bool_to_float.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x bool #t) (define/assign y float x))))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x bool #t) (define/assign y int x))))))

;; conformance_suite/upcast_int_to_float.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x int 2) (define/assign y float x))))))
