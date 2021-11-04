#lang racket
(require redex)
(require "desugar.rkt")
(require "compile.rkt")

;; conformance_suite/PyDict_delete_bad_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "other")))))))

;; conformance_suite/PyDict_delete_good_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")))))))

;; conformance_suite/PyDict_delete_then_lookup.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar")) (expr (subscript x "bar")))))))

;; conformance_suite/PyDict_insert.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") dynamic "hello"))))))

;; conformance_suite/PyDict_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))))))))

;; conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "other")))))))

;; conformance_suite/PyDict_lookup_good_key.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar")))))))

;; conformance_suite/PyDict_update.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") dynamic "hello"))))))

;; conformance_suite/bool_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x bool #t))))))

;; conformance_suite/empty_program.py
(test-match SP-compiled program- (term (compile-program (desugar-program ()))))

;; conformance_suite/float_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x float 2.3))))))

;; conformance_suite/int_is_inhabitable.py
(test-match SP-compiled program- (term (compile-program (desugar-program ((define/assign x int 42))))))
