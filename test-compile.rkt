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

;; conformance_suite/upcast_bool_to_float.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x bool #t) (define/assign y float x))))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x bool #t) (define/assign y int x))))))

;; conformance_suite/upcast_int_to_float.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x int 2) (define/assign y float x))))))
