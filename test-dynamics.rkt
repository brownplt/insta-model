#lang racket
(require redex)
(require redex-abbrevs)
(require "desugar.rkt")
(require "compile.rkt")
(require "dynamics.rkt")

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "other"))))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar"))))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")) (expr (subscript x "bar"))))))))

;; conformance_suite/PyDict_insert.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") dynamic "hello")))))))

;; conformance_suite/PyDict_insert_then_lookup.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax)) (define/assign (subscript x "foo") dynamic 42) (expr (subscript x "foo"))))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2)))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "other"))))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar"))))))))

;; conformance_suite/PyDict_update.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") dynamic "hello")))))))

;; conformance_suite/PyDict_update_then_lookup.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax ("foo" 2))) (define/assign (subscript x "foo") dynamic 3) (assert (is (subscript x "foo") 3))))))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((define/assign x bool #t)))))))

;; conformance_suite/downcast_float_to_int_neg.py
(test-match SP-dynamics (error) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (define/assign x float 2.3) (define/assign y Any x) (define/assign z int y)))))))

;; conformance_suite/downcast_float_to_int_pos.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((import-from "typing" ("Any")) (define/assign x float 2) (define/assign y Any x) (define/assign z int y)))))))

;; conformance_suite/empty_program.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ())))))

;; conformance_suite/float_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((define/assign x float 2.3)))))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((define/assign x int 42)))))))

;; conformance_suite/upcast_bool_to_float.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((define/assign x bool #t) (define/assign y float x)))))))

;; conformance_suite/upcast_bool_to_int.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((define/assign x bool #t) (define/assign y int x)))))))

;; conformance_suite/upcast_int_to_float.py
(test-match SP-dynamics (begin (expr v) ...) (term (calc (compile-program (desugar-program ((define/assign x int 2) (define/assign y float x)))))))
