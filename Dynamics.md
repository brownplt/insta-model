# Dynamic Semantics

This file describes the dynamic semantics (runtime) of the idealized Static Python.

## Optional

`Optional[T]` checks accept `None` and instances of `T`, but not others.

- [optional_is_inhabitable_none_rt.py](conformance_suite/optional_is_inhabitable_none_rt.py)
- [optional_is_inhabitable_nonnone_rt.py](conformance_suite/optional_is_inhabitable_nonnone_rt.py)
- [optional_is_inhabitable_other_rt.py](conformance_suite/optional_is_inhabitable_other_rt.py)

## PyDict

`PyDict` is inhabitable.

- [PyDict_is_inhabitable.py](conformance_suite/PyDict_is_inhabitable.py)

Inserting `PyDict` entries is allowed.

- [PyDict_insert.py](conformance_suite/PyDict_insert.py)

Updating `PyDict` entries is allowed.

- [PyDict_update.py](conformance_suite/PyDict_update.py)

Deleting `PyDict` entries with good keys is allowed.

- [PyDict_delete_good_key.py](conformance_suite/PyDict_delete_good_key.py)

Deleting `PyDict` entries with bad keys is allowed at compile time, but should fail at runtime.

- [PyDict_delete_bad_key.py](conformance_suite/PyDict_delete_bad_key.py)

Looking up `PyDict` entries with good keys is allowed.

- [PyDict_lookup_good_key.py](conformance_suite/PyDict_lookup_good_key.py)

Looking up `PyDict` entries with bad keys is allowed at compile time, but should fail at runtime.

- [PyDict_lookup_bad_key.py](conformance_suite/PyDict_lookup_bad_key.py)

After deletion, looking up the key should fail.

- [PyDict_delete_then_lookup.py](conformance_suite/PyDict_delete_then_lookup.py)

After insertion, looking up the key should succeed.

- [PyDict_insert_then_lookup.py](conformance_suite/PyDict_insert_then_lookup.py)

After updating, looking up the key should give the new value.

- [PyDict_update_then_lookup.py](conformance_suite/PyDict_update_then_lookup.py)

## CheckedDict

`CheckedDict` is inhabitable.

- [CheckedDict_is_inhabitable.py](conformance_suite/CheckedDict_is_inhabitable.py)
- [CheckedDict_from_good_dict.py](conformance_suite/CheckedDict_from_good_dict.py)

`CheckedDict[T1, T2].__getitem__` works.

- [CheckedDict_lookup_good_key.py](conformance_suite/CheckedDict_lookup_good_key.py)
- [CheckedDict_lookup_bad_key.py](conformance_suite/CheckedDict_lookup_bad_key.py)

`CheckedDict[T1, T2].__getitem__` checks its input(s) dynamically.

- [CheckedDict_lookup_dom_dyn_good.py](conformance_suite/CheckedDict_lookup_dom_dyn_good.py)
- [CheckedDict_lookup_dom_dyn_bad_key.py](conformance_suite/CheckedDict_lookup_dom_dyn_bad_key.py)

`CheckedDict[T1, T2].__setitem__` works.

- [CheckedDict_update.py](conformance_suite/CheckedDict_update.py)

`CheckedDict[T1, T2].__setitem__` checks its input(s) dynamically.

- [CheckedDict_update_dom_dyn_good.py](conformance_suite/CheckedDict_update_dom_dyn_good.py)
- [CheckedDict_update_dom_dyn_bad_key.py](conformance_suite/CheckedDict_update_dom_dyn_bad_key.py)
- [CheckedDict_update_dom_dyn_bad_val.py](conformance_suite/CheckedDict_update_dom_dyn_bad_val.py)

`CheckedDict[T1, T2].__delitem__` works.

- [CheckedDict_delete_good_key.py](conformance_suite/CheckedDict_delete_good_key.py)
- [CheckedDict_delete_bad_key.py](conformance_suite/CheckedDict_delete_bad_key.py)

`CheckedDict[T1, T2].__delitem__` checks its input(s) dynamically.

- [CheckedDict_delete_dom_dyn_good.py](conformance_suite/CheckedDict_delete_dom_dyn_good.py)
- [CheckedDict_delete_dom_dyn_bad_key.py](conformance_suite/CheckedDict_delete_dom_dyn_bad_key.py)

The CheckedDict constructor checks its argument.

- [CheckedDict_from_nondict.py](conformance_suite/CheckedDict_from_nondict.py)
- [CheckedDict_from_bad_dict.py](conformance_suite/CheckedDict_from_bad_dict.py)

## PyDicts and CheckedDicts

`PyDict`s are not `CheckedDict`s, and vice versa.

- [PyDict_to_CheckedDict_forward.py](conformance_suite/PyDict_to_CheckedDict_forward.py)
- [PyDict_to_CheckedDict_backward.py](conformance_suite/PyDict_to_CheckedDict_backward.py)

## Procedure

Procedures work.

- [procedure_works.py](conformance_suite/procedure_works.py)

Procedures check arity.

- [procedure_check_arity_dynamically.py](conformance_suite/procedure_check_arity_dynamically.py)

Procedures check argument types.

- [procedure_check_argument_type_dynamically.py](conformance_suite/procedure_check_argument_type_dynamically.py)

Procedures check return types.

- [procedure_check_return_type_dynamically.py](conformance_suite/procedure_check_return_type_dynamically.py)

## Runtime class checks

Casting to a super class is okay.

- [upcast_bool_to_int.py](conformance_suite/upcast_bool_to_int.py)
- [upcast_C2_to_C1.py](conformance_suite/upcast_C1_to_C2.py)

Casting to a sub class may or maynot be okay

- [downcast_int_to_bool_pos.py](conformance_suite/downcast_int_to_bool_pos.py)
- [downcast_int_to_bool_neg.py](conformance_suite/downcast_int_to_bool_neg.py)
- [downcast_C1_to_C2_pos.py](conformance_suite/downcast_C1_to_C2_pos.py)
- [downcast_C1_to_C2_neg.py](conformance_suite/downcast_C1_to_C2_neg.py)

## Members

Constructors work.

- [instance_creation.py](conformance_suite/instance_creation.py)

Fields are checked when initilized and updated.

- [field_init.py](conformance_suite/field_init.py)
- [field_update.py](conformance_suite/field_update.py)

Class variables in sub-classes shadow variables in parent classes.

- [class_variables_may_shadow.py](conformance_suite/class_variables_may_shadow.py)

Methods are class variables.

- [methods_can_be_declared_as_class_variables.py](conformance_suite/methods_can_be_declared_as_class_variables.py)
- [method_from_def.py](conformance_suite/method_from_def.py)
- [method_from_lambda.py](conformance_suite/method_from_lambda.py)

Methods override.

- [method_override_exact.py](conformance_suite/method_override_exact.py)
- [method_override_inexact.py](conformance_suite/method_override_inexact.py)
- [method_override_dynamic.py](conformance_suite/method_override_dynamic.py)

Methods are generative.

- [method_generative.py](conformance_suite/method_generative.py)

Programmers can't introduce new fields.

- [static_class_update_dynamic_field.py](conformance_suite/static_class_update_dynamic_field.py)
- [static_class_update_static_field.py](conformance_suite/static_class_update_static_field.py)

## Try-except

Body runs.

- [try_except_basic.py](conformance_suite/try_except_basic.py)

Exceptions are caught properly.

- [try_except_catch_same_class.py](conformance_suite/try_except_catch_same_class.py)
- [try_except_catch_sub_class.py](conformance_suite/try_except_catch_sub_class.py)

The else case runs when no exception is raised.

- [try_except_catch_else_no_exn.py](conformance_suite/try_except_catch_else_no_exn.py)
- [try_except_catch_else_some_exn.py](conformance_suite/try_except_catch_else_some_exn.py)

The final block is executed with and without exception.

- [try_except_catch_final_no_exn.py](conformance_suite/try_except_catch_final_no_exn.py)
- [try_except_catch_final_some_exn.py](conformance_suite/try_except_catch_final_some_exn.py)

## While loop

Basic use.

- [while-loop_basic.py](conformance_suite/while-loop_basic.py)

While-else.

- [while-loop_else.py](conformance_suite/while-loop_else.py)

## For loop

Basic use.

- [for-loop_basic.py](conformance_suite/for-loop_basic.py)

The `else` lines are executed "if the loop finishes normally" [Python ast.For](https://docs.python.org/3/library/ast.html#ast.For)

- [for-loop_else_raise_exit.py](conformance_suite/for-loop_else_raise_exit.py)
- [for-loop_else_normal_exit.py](conformance_suite/for-loop_else_normal_exit.py)

## For loop and base types

`list`s and `str`s are iterable.

- [iterate-list.py](conformance_suite/iterate-list.py)
- [iterate-str.py](conformance_suite/iterate-str.py)
