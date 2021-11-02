#lang racket
(require "desugar.rkt")
(require redex)

;; ./conformance_suite/CheckedDict_delete_neg.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x 2))))))

;; ./conformance_suite/CheckedDict_delete_pos.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (delete (subscript x "foo"))))))

;; ./conformance_suite/CheckedDict_from_dict.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (2 "a") (3 "b"))))))))

;; ./conformance_suite/CheckedDict_from_nondict.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax int str)) ((subscript CheckedDict (tuple-syntax int str)) 42))))))

;; ./conformance_suite/CheckedDict_lookup_key_neg.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (expr (subscript x 2))))))

;; ./conformance_suite/CheckedDict_lookup_key_pos.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (expr (subscript x "foo"))))))

;; ./conformance_suite/CheckedDict_lookup_val_neg.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y str (subscript x "foo"))))))

;; ./conformance_suite/CheckedDict_lookup_val_pos.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign y int (subscript x "foo"))))))

;; ./conformance_suite/CheckedDict_update_key_neg.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x 2) dynamic 3)))))

;; ./conformance_suite/CheckedDict_update_key_pos.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic 3)))))

;; ./conformance_suite/CheckedDict_update_val_neg.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic "2")))))

;; ./conformance_suite/CheckedDict_update_val_pos.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict" "CheckedDict")) (define/assign x (subscript CheckedDict (tuple-syntax str int)) ((subscript CheckedDict (tuple-syntax str int)) (dict-syntax ("foo" 1)))) (define/assign (subscript x "bar") dynamic 2)))))

;; ./conformance_suite/None_is_inhabitable.py
(test-match SP-core program- (term (desugar-program ((define/assign x None None)))))

;; ./conformance_suite/PyDict_delete.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (delete (subscript x "bar"))))))

;; ./conformance_suite/PyDict_insert.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "new") dynamic "hello")))))

;; ./conformance_suite/PyDict_is_inhabitable.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2)))))))

;; ./conformance_suite/PyDict_lookup_bad_key.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar"))))))

;; ./conformance_suite/PyDict_lookup_good_key.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (expr (subscript x "bar"))))))

;; ./conformance_suite/PyDict_update.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("PyDict")) (define/assign x PyDict (dict-syntax (1 "foo") ("bar" 2))) (define/assign (subscript x "bar") dynamic "hello")))))

;; ./conformance_suite/assign_declared_field_neg.py
(test-match SP-core program- (term (desugar-program ((class B (object) (field "x" str)) (class C (B)) (def f ((c C)) dynamic ((define/assign (attribute c "x") dynamic 42)))))))

;; ./conformance_suite/assign_declared_field_pos.py
(test-match SP-core program- (term (desugar-program ((class B (object) (field "x" int)) (class C (B)) (def f ((c C)) dynamic ((define/assign (attribute c "x") dynamic 42)))))))

;; ./conformance_suite/bool_is_a_subtype_of_int_neg.py
(test-match SP-core program- (term (desugar-program ((define/assign x bool 42)))))

;; ./conformance_suite/bool_is_a_subtype_of_int_pos.py
(test-match SP-core program- (term (desugar-program ((define/assign x int #t)))))

;; ./conformance_suite/bool_is_inhabitable.py
(test-match SP-core program- (term (desugar-program ((define/assign x bool #t)))))

;; ./conformance_suite/child_is_a_subtype_of_parent_neg.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (C)) (define/assign x D (C))))))

;; ./conformance_suite/child_is_a_subtype_of_parent_pos.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (C)) (define/assign x C (D))))))

;; ./conformance_suite/delete_declared_field.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" str)) (def f ((c C)) dynamic ((delete (attribute c "x"))))))))

;; ./conformance_suite/delete_undeclared_field.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (def f ((c C)) dynamic ((delete (attribute c "x"))))))))

;; ./conformance_suite/dynamic_as_CheckedDict.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def id ((x dynamic)) (subscript CheckedDict (tuple-syntax str int)) ((return x)))))))

;; ./conformance_suite/dynamic_as_callable.py
(test-match SP-core program- (term (desugar-program ((def f ((x dynamic)) dynamic ((return (x (x 2) (x "foo")))))))))

;; ./conformance_suite/dynamic_as_int.py
(test-match SP-core program- (term (desugar-program ((def id ((x dynamic)) int ((return x)))))))

;; ./conformance_suite/dynamic_as_user-defined_class.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (def id ((x dynamic)) C ((return x)))))))

;; ./conformance_suite/empty_program.py
(test-match SP-core program- (term (desugar-program ())))

;; ./conformance_suite/init_checks_arity.py
(test-match SP-core program- (term (desugar-program ((class Person (object) (method "__init__" self ((name str) (age int)) dynamic pass)) (define/assign p1 dynamic (Person "Alice" 21 #f))))))

;; ./conformance_suite/init_checks_type.py
(test-match SP-core program- (term (desugar-program ((class Person (object) (method "__init__" self ((name str) (age int)) dynamic pass)) (define/assign p1 dynamic (Person "Alice" "21"))))))

;; ./conformance_suite/insert_new_field.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (def f ((c C)) dynamic ((define/assign (attribute c "x") dynamic 42)))))))

;; ./conformance_suite/int_is_inhabitable.py
(test-match SP-core program- (term (desugar-program ((define/assign x int 42)))))

;; ./conformance_suite/lookup_declared_field_neg.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" str)) (def expectInt ((i int)) dynamic (pass)) (def f ((c C)) dynamic ((return (expectInt (attribute c "x")))))))))

;; ./conformance_suite/lookup_declared_field_pos.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" int)) (def expectInt ((i int)) dynamic (pass)) (def f ((c C)) dynamic ((return (expectInt (attribute c "x")))))))))

;; ./conformance_suite/lookup_parent_field_neg.py
(test-match SP-core program- (term (desugar-program ((class B (object) (field "x" str)) (class C (B)) (def expectInt ((i int)) dynamic (pass)) (def f ((c C)) dynamic ((return (expectInt (attribute c "x")))))))))

;; ./conformance_suite/lookup_parent_field_pos.py
(test-match SP-core program- (term (desugar-program ((class B (object) (field "x" int)) (class C (B)) (def expectInt ((i int)) dynamic (pass)) (def f ((c C)) dynamic ((return (expectInt (attribute c "x")))))))))

;; ./conformance_suite/lookup_undeclared_field.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (def expectInt ((i int)) dynamic (pass)) (def f ((c C)) dynamic ((return (expectInt (attribute c "x")))))))))

;; ./conformance_suite/methods_check_input_arity.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "m" self ((x dynamic)) dynamic pass)) (def f () dynamic ((expr ((attribute (C) "m") 1 2))))))))

;; ./conformance_suite/methods_check_input_types.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "m" self ((x int)) dynamic (return None))) (expr ((attribute (C) "m") "foo"))))))

;; ./conformance_suite/methods_check_output_types.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "m" self () int (return "foo")))))))

;; ./conformance_suite/methods_work.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "m" self ((x int)) str (return "foo"))) (define/assign s str ((attribute (C) "m") 42))))))

;; ./conformance_suite/optional_is_inhabitable_1.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (define/assign x (subscript Optional int) 42)))))

;; ./conformance_suite/optional_is_inhabitable_2.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (define/assign x (subscript Optional int) None)))))

;; ./conformance_suite/override_instance_field_with_imprecise_type.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (field "x" str)) (class D (C) (field "x" Any))))))

;; ./conformance_suite/override_instance_field_with_incompatible_type.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (field "x" str))))))

;; ./conformance_suite/override_instance_field_with_method.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (method "x" self () dynamic pass))))))

;; ./conformance_suite/override_instance_field_with_precise_type.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Any")) (class C (object) (field "x" Any)) (class D (C) (field "x" str))))))

;; ./conformance_suite/override_instance_field_with_same_type.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (field "x" int))))))

;; ./conformance_suite/override_instance_field_with_subtype.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (field "x" bool))))))

;; ./conformance_suite/override_instance_field_with_suptype.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" bool)) (class D (C) (field "x" int))))))

;; ./conformance_suite/override_instance_method_contravariant_inputs_neg.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self ((x C)) None (return None))) (class B (A) (method "m" self ((x D)) None (return None)))))))

;; ./conformance_suite/override_instance_method_contravariant_inputs_pos.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self ((x D)) None (return None))) (class B (A) (method "m" self ((x C)) None (return None)))))))

;; ./conformance_suite/override_instance_method_covariant_output_neg.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self () D (return (C)))) (class B (A) (method "m" self () C (return (C))))))))

;; ./conformance_suite/override_instance_method_covariant_output_pos.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (C)) (class A (object) (method "m" self () C (return (C)))) (class B (A) (method "m" self () D (return (D))))))))

;; ./conformance_suite/override_instance_method_with_field.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "x" self () dynamic pass)) (class D (C) (field "x" int))))))

;; ./conformance_suite/str_is_inhabitable.py
(test-match SP-core program- (term (desugar-program ((define/assign x str "hello")))))

;; ./conformance_suite/subclass_builtin.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("cast")) (class C (int)) (define/assign x C (C 42)) (define/assign y int x)))))

;; ./conformance_suite/test_assign_generic_optional_2.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f () dynamic ((define/assign x Optional (bin-op + 42 1))))))))

;; ./conformance_suite/test_assign_subtype_handling.py
(test-match SP-core program- (term (desugar-program ((class B (object)) (class D (B)) (def f () dynamic ((define/assign b B (B)) (define/assign b dynamic (D)) (define/assign b dynamic (B))))))))

;; ./conformance_suite/test_assign_test_var.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int ((if (is x None) ((define/assign x dynamic 1)) ()) (return x)))))))

;; ./conformance_suite/test_assign_type_propagation.py
(test-match SP-core program- (term (desugar-program ((def test () int ((define/assign x dynamic 5) (return x)))))))

;; ./conformance_suite/test_bool_cast.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("cast")) (class D (object)) (def f ((x dynamic)) bool ((define/assign y bool (cast bool x)) (return y)))))))

;; ./conformance_suite/test_bool_int.py
(test-match SP-core program- (term (desugar-program ((def f () dynamic ((define/assign x int #t) (return x)))))))

;; ./conformance_suite/test_cast_unknown_type.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("cast")) (def f () dynamic ((expr (cast abc 42))))))))

;; ./conformance_suite/test_cast_wrong_args.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("cast")) (def f () dynamic ((expr (cast 42))))))))

;; ./conformance_suite/test_clen_bad_arg.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("clen")) (def f ((l dynamic)) dynamic ((expr (clen l))))))))

;; ./conformance_suite/test_compile_dict_get_typed.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic ((define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (42 "abc")))) (define/assign y (or-syntax str None) ((attribute x "get") 42))))))))

;; ./conformance_suite/test_compile_dict_setdefault_typed.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("CheckedDict")) (def testfunc () dynamic ((define/assign x dynamic ((subscript CheckedDict (tuple-syntax int str)) (dict-syntax (42 "abc")))) (define/assign y (or-syntax str None) ((attribute x "setdefault") 100 "foo"))))))))

;; ./conformance_suite/test_duplicate_function_replaces_class.py
(test-match SP-core program- (term (desugar-program ((class X (object)) (def X () dynamic (pass))))))

;; ./conformance_suite/test_duplicate_function_replaces_function.py
(test-match SP-core program- (term (desugar-program ((def f () dynamic (pass)) (def f () dynamic (pass))))))

;; ./conformance_suite/test_final.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Final")) (define/assign x (subscript Final int) 3735928559)))))

;; ./conformance_suite/test_final_generic.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Final")) (define/assign x (subscript Final int) 3735928559)))))

;; ./conformance_suite/test_final_generic_types.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Final")) (def g ((i int)) int ((return i))) (def f () int ((define/assign x (subscript Final int) 3735928559) (return (g x))))))))

;; ./conformance_suite/test_frozenset_constant.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("inline")) (def i ((s str)) bool ((return (in i (set-syntax "a" "b"))))) (def t () bool ((return (i "p"))))))))

;; ./conformance_suite/test_if_else_optional.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (define/assign (attribute self "field") dynamic self))) (def g ((x C)) dynamic (pass)) (def f ((x (subscript Optional C)) (y (subscript Optional C))) dynamic ((if (is x None) ((define/assign x dynamic y) (if (is x None) ((return None)) ((return (g x))))) ((return (g x)))) (return None)))))))

;; ./conformance_suite/test_if_else_optional_return.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (define/assign (attribute self "field") dynamic self))) (def f ((x (subscript Optional C))) dynamic ((if (is x None) ((return 0)) ()) (return (attribute x "field"))))))))

;; ./conformance_suite/test_if_else_optional_return_in_else.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int ((if (is-not x None) (pass) ((return 2))) (return x)))))))

;; ./conformance_suite/test_if_else_optional_return_in_else_assignment_in_if.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int ((if (is x None) ((define/assign x dynamic 1)) ((return 2))) (return x)))))))

;; ./conformance_suite/test_if_else_optional_return_in_if_assignment_in_else.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f ((x (subscript Optional int))) int ((if (is-not x None) ((return 2)) ((define/assign x dynamic 1))) (return x)))))))

;; ./conformance_suite/test_if_optional.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (define/assign (attribute self "field") dynamic 42))) (def f ((x (subscript Optional C))) dynamic ((if (is-not x None) ((return (attribute x "field"))) ()) (return None)))))))

;; ./conformance_suite/test_if_optional_cond.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (define/assign (attribute self "field") dynamic 42))) (def f ((x (subscript Optional C))) dynamic ((return (if (is-not x None) (attribute x "field") None))))))))

;; ./conformance_suite/test_if_optional_dependent_conditions.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "__init__" self () dynamic (define/assign (attribute self "field") (subscript Optional C) None))) (def f ((x (subscript Optional C))) C ((if (bool-op and (is-not x None) (is-not (attribute x "field") None)) ((return x)) ()) (if (is x None) ((return (C))) ()) (return x)))))))

;; ./conformance_suite/test_incompat_override.py
(test-match SP-core program- (term (desugar-program ((class C (object) (field "x" int)) (class D (C) (method "x" self () dynamic pass))))))

;; ./conformance_suite/test_incompat_override_method_arg_name.py
(test-match SP-core program- (term (desugar-program ((class A (object) (method "m" self ((x str)) int (return 42))) (class B (A) (method "m" self ((y str)) int (return 0)))))))

;; ./conformance_suite/test_incompat_override_method_arg_type.py
(test-match SP-core program- (term (desugar-program ((class A (object) (method "m" self ((x str)) int (return 42))) (class B (A) (method "m" self ((x int)) int (return 0)))))))

;; ./conformance_suite/test_incompat_override_method_arg_type_okay.py
(test-match SP-core program- (term (desugar-program ((class A (object) (method "m" self ((x str)) int (return 42))) (class B (A) (method "m" self ((x object)) int (return 0)))))))

;; ./conformance_suite/test_incompat_override_method_num_args.py
(test-match SP-core program- (term (desugar-program ((class A (object) (method "m" self () int (return 42))) (class B (A) (method "m" self ((x int)) int (return 0)))))))

;; ./conformance_suite/test_incompat_override_method_ret_type.py
(test-match SP-core program- (term (desugar-program ((class A (object) (method "m" self () str (return "hello"))) (class B (A) (method "m" self () int (return 0)))))))

;; ./conformance_suite/test_inline_arg_type_mismatch.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("inline")) (def f ((x int)) bool ((return (== x 1)))) (def g ((arg str)) bool ((return (f arg))))))))

;; ./conformance_suite/test_inline_return_type_mismatch.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("inline")) (def f () int ((return 1))) (def g () str ((return (f))))))))

;; ./conformance_suite/test_multiple_dynamic_base_class.py
(test-match SP-core program- (term (desugar-program ((import-from "something" ("A" "B")) (class C (A B) (method "__init__" self () dynamic pass))))))

;; ./conformance_suite/test_narrow_or.py
(test-match SP-core program- (term (desugar-program ((def f ((x (or-syntax int None))) int ((if (bool-op or (is x None) (> x 1)) ((define/assign x dynamic 1)) ()) (return x)))))))

;; ./conformance_suite/test_none_compare.py
(test-match SP-core program- (term (desugar-program ((def f ((x (or-syntax int None))) dynamic ((if (> x 1) ((define/assign x dynamic 1)) ()) (return x)))))))

;; ./conformance_suite/test_none_compare_reverse.py
(test-match SP-core program- (term (desugar-program ((def f ((x (or-syntax int None))) dynamic ((if (> 1 x) ((define/assign x dynamic 1)) ()) (return x)))))))

;; ./conformance_suite/test_optional_assign.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class C (object) (method "f" self ((x (subscript Optional "C"))) dynamic (if (is x None) ((return self)) ((define/assign p (subscript Optional "C") x)))))))))

;; ./conformance_suite/test_optional_assign_none.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class B (object)) (def f ((x (subscript Optional B))) dynamic ((define/assign a (subscript Optional B) None)))))))

;; ./conformance_suite/test_optional_assign_subclass.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class B (object)) (class D (B)) (def f ((x D)) dynamic ((define/assign a (subscript Optional B) x)))))))

;; ./conformance_suite/test_optional_assign_subclass_opt.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (class B (object)) (class D (B)) (def f ((x (subscript Optional D))) dynamic ((define/assign a (subscript Optional B) x)))))))

;; ./conformance_suite/test_optional_subscript_error.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f ((a (subscript Optional int))) dynamic ((expr (subscript a 1))))))))

;; ./conformance_suite/test_optional_unary_error.py
(test-match SP-core program- (term (desugar-program ((import-from "typing" ("Optional")) (def f ((a (subscript Optional int))) dynamic ((expr (unary-op - a))))))))

;; ./conformance_suite/test_redefine_local_type.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (object)) (def f () dynamic ((define/assign x C (C)) (define/assign x D (D))))))))

;; ./conformance_suite/test_redefine_type.py
(test-match SP-core program- (term (desugar-program ((class C (object)) (class D (object)) (def f ((a dynamic)) dynamic ((define/assign x C (C)) (define/assign x D (D))))))))

;; ./conformance_suite/test_static_import_star.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("*"))))))

;; ./conformance_suite/test_static_import_unknown.py
(test-match SP-core program- (term (desugar-program ((import-from "__static__" ("does_not_exist"))))))

;; ./conformance_suite/test_type_of_or.py
(test-match SP-core program- (term (desugar-program ((def f ((x int) (y str)) (or-syntax int str) ((return (bool-op or x y))))))))

;; ./conformance_suite/test_type_type_final.py
(test-match SP-core program- (term (desugar-program ((class A (type))))))

;; ./conformance_suite/test_unannotated_assign_no_later_declare.py
(test-match SP-core program- (term (desugar-program ((def f ((flag dynamic)) dynamic ((define/assign x dynamic None) (if flag ((define/assign x str "foo")) ())))))))

;; ./conformance_suite/test_verify_positional_args.py
(test-match SP-core program- (term (desugar-program ((def x ((a int) (b str)) None (pass)) (expr (x "a" 2))))))

;; ./conformance_suite/test_verify_positional_args_failure_method.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "x" self ((a int) (b str)) None pass)) (expr ((attribute (C) "x") "a" 2))))))

;; ./conformance_suite/test_verify_positional_args_method.py
(test-match SP-core program- (term (desugar-program ((class C (object) (method "x" self ((a int) (b str)) None pass)) (expr ((attribute (C) "x") 2 "hi"))))))

;; ./conformance_suite/test_verify_positional_args_unordered.py
(test-match SP-core program- (term (desugar-program ((def x ((a int) (b str)) None ((return (y a b)))) (def y ((a int) (b str)) None (pass))))))

;; ./conformance_suite/test_verify_too_many_args.py
(test-match SP-core program- (term (desugar-program ((def x () dynamic ((return 42))) (expr (x 1))))))