# Static Semantics

This file describes the static semantics (type system) of the idealized Static Python.

## Empty Program

The empty program should type check.

- [empty_program.py](conformance_suite/empty_program.py)

## Base Types

`bool` is inhabitable.

- [bool_is_inhabitable.py](conformance_suite/bool_is_inhabitable.py)

`int` is inhabitable.

- [int_is_inhabitable.py](conformance_suite/int_is_inhabitable.py)

`str` is inhabitable.

- [str_is_inhabitable.py](conformance_suite/str_is_inhabitable.py)

## Optional

`Optional[T]` is inhabitable.

- [optional_is_inhabitable_1.py](conformance_suite/optional_is_inhabitable_1.py)
- [optional_is_inhabitable_2.py](conformance_suite/optional_is_inhabitable_2.py)

TODO: more `optional` tests.

## PyDict

`PyDict` is inhabitable.

- [PyDict_is_inhabitable.py](conformance_suite/PyDict_is_inhabitable.py)

Inserting `PyDict` entries is allowed.

- [PyDict_insert.py](conformance_suite/PyDict_insert.py)

Updating `PyDict` entries is allowed.

- [PyDict_update.py](conformance_suite/PyDict_update.py)

Deleting `PyDict` entries with good keys is allowed.

- [PyDict_delete_good_key.py](conformance_suite/PyDict_delete_good_key.py)
- 
Deleting `PyDict` entries with bad keys is allowed.

- [PyDict_delete_bad_key.py](conformance_suite/PyDict_delete_bad_key.py)

Looking up `PyDict` entries with good keys is allowed.

- [PyDict_lookup_good_key.py](conformance_suite/PyDict_lookup_good_key.py)

Looking up `PyDict` entries with bad keys is allowed.

- [PyDict_lookup_bad_key.py](conformance_suite/PyDict_lookup_bad_key.py)

## CheckedDict[T0, T1]

`CheckedDict` must be constructed from `dict`.
- [CheckedDict_from_nondict.py](conformance_suite/CheckedDict_from_nondict.py)
- [CheckedDict_from_bad_dict.py](conformance_suite/CheckedDict_from_bad_dict.py)
- [CheckedDict_from_good_dict.py](conformance_suite/CheckedDict_from_good_dict.py)

Looking up `CheckedDict[T0, T1]` expects a `T0` key.
- [CheckedDict_lookup_key_neg.py](conformance_suite/CheckedDict_lookup_key_neg.py)
- [CheckedDict_lookup_key_pos.py](conformance_suite/CheckedDict_lookup_key_pos.py)

Looking up `CheckedDict[T0, T1]` returns a `T1` value.
- [CheckedDict_lookup_val_neg.py](conformance_suite/CheckedDict_lookup_val_neg.py)
- [CheckedDict_lookup_val_pos.py](conformance_suite/CheckedDict_lookup_val_pos.py)

Updating a `CheckedDict[T0, T1]` checks the key.
- [CheckedDict_update_key_neg.py](conformance_suite/CheckedDict_update_key_neg.py)
- [CheckedDict_update_key_pos.py](conformance_suite/CheckedDict_update_key_pos.py)

Updating a `CheckedDict[T0, T1]` checks the value.
- [CheckedDict_update_val_neg.py](conformance_suite/CheckedDict_update_val_neg.py)
- [CheckedDict_update_val_pos.py](conformance_suite/CheckedDict_update_val_pos.py)

Deleting `CheckedDict[T0, T1]` entries checks the key.
- [CheckedDict_delete_neg.py](conformance_suite/CheckedDict_delete_neg.py)
- [CheckedDict_delete_pos.py](conformance_suite/CheckedDict_delete_pos.py)

The key type of CheckedDicts can be `Optional[T]`.

- [CheckedDict_val_can_be_Optional.py](conformance_suite/CheckedDict_val_can_be_Optional.py)
- [CheckedDict_key_can_be_Optional.py](conformance_suite/CheckedDict_key_can_be_Optional.py)

## Procedures

Procedure check arity

- [procedure_check_arity_statically.py](conformance_suite/procedure_check_arity_statically.py)

Procedure check argument types.

- [procedure_check_argument_type_statically.py](conformance_suite/procedure_check_argument_type_statically.py)

Procedure check return types.

- [procedure_check_return_type_statically.py](conformance_suite/procedure_check_return_type_statically.py)

## Classes

### Construct instances

`__init__` checks its arity.

- [init_checks_arity.py](conformance_suite/init_checks_arity.py)

`__init__` checks its argument types.

- [init_checks_type.py](conformance_suite/init_checks_type.py)

### Inheritance/subclassing

Inheriting builtin classes is allowed.

- [subclass_builtin_atomic.py](conformance_suite/subclass_builtin_atomic.py)
- [subclass_builtin_generic.py](conformance_suite/subclass_builtin_generic.py)

Overriding a method with a field is a static error.

- [override_instance_method_with_field.py](conformance_suite/override_instance_method_with_field.py)

Overriding a field with a method is a static error.

- [override_instance_field_with_method.py](conformance_suite/override_instance_field_with_method.py)

Overriding a field is a static error.

- [override_instance_field.py](conformance_suite/override_instance_field.py) TODO: Waiting for a reply to https://github.com/facebookincubator/cinder/issues/39

Overriding a method requires that the new output type is a subtype of the old one.

- [override_instance_method_covariant_output_neg.py](conformance_suite/override_instance_method_covariant_output_neg.py)
- [override_instance_method_covariant_output_pos.py](conformance_suite/override_instance_method_covariant_output_pos.py)

Overriding a method requires that the new input type is a supertype of the old one.

- [override_instance_method_contravariant_inputs_neg.py](conformance_suite/override_instance_method_contravariant_inputs_neg.py)
- [override_instance_method_contravariant_inputs_pos.py](conformance_suite/override_instance_method_contravariant_inputs_pos.py)

Overriding a method permits a possibly less precise domain.

- [override_instance_method_domain_gain_precision.py](conformance_suite/override_instance_method_domain_gain_precision.py)
- [override_instance_method_domain_lose_precision.py](conformance_suite/override_instance_method_domain_lose_precision.py)

Overriding a method permits a possibly more precise codomain.

- [override_instance_method_codomain_gain_precision.py](conformance_suite/override_instance_method_codomain_gain_precision.py)
- [override_instance_method_codomain_lose_precision.py](conformance_suite/override_instance_method_codomain_lose_precision.py)

### Classes as types

Classes aren't first-class. Unresolved types are NOT checked.

- [classes_are_not_first-class.py](conformance_suite/classes_are_not_first-class.py)

## Instances

Looking up a declared field has the expected type.

- [lookup_declared_field_pos.py](conformance_suite/lookup_declared_field_pos.py)
- [lookup_declared_field_neg.py](conformance_suite/lookup_declared_field_neg.py)

Looking up an undeclared field falls back to `dynamic`.

- [lookup_undeclared_field.py](conformance_suite/lookup_undeclared_field.py)

Looking up a declared parent field has the expected type.

- [lookup_parent_field_pos.py](conformance_suite/lookup_parent_field_pos.py)
- [lookup_parent_field_neg.py](conformance_suite/lookup_parent_field_neg.py)

Assigning to a declared field requires matched type, even if that field is declared in the parent class.

- [assign_declared_field_pos.py](conformance_suite/assign_declared_field_pos.py)
- [assign_declared_field_neg.py](conformance_suite/assign_declared_field_neg.py)

Inserting new fields is allowed.

- [insert_new_field.py](conformance_suite/insert_new_field.py)

Deleting fields is allowed.

- [delete_declared_fields.py](conformance_suite/delete_declared_fields.py)
- [delete_undeclared_fields.py](conformance_suite/delete_undeclared_fields.py)

Declared methods check expected things.

- [methods_work.py](conformance_suite/methods_work.py)
- [methods_check_input_arity.py](conformance_suite/methods_check_input_arity.py)
- [methods_check_input_types.py](conformance_suite/methods_check_input_types.py)
- [methods_check_output_types.py](conformance_suite/methods_check_output_types.py)

## Subtyping

`bool` is a subtype of `int`

- [bool_is_a_subtype_of_int_neg.py](conformance_suite/bool_is_a_subtype_of_int_neg.py)
- [bool_is_a_subtype_of_int_pos.py](conformance_suite/bool_is_a_subtype_of_int_pos.py)

If `C` is a subclass of `D`, then `C` is a subtype of `D`.

- [child_is_a_subtype_of_parent_neg.py](conformance_suite/child_is_a_subtype_of_parent_neg.py)
- [child_is_a_subtype_of_parent_pos.py](conformance_suite/child_is_a_subtype_of_parent_pos.py)

`CheckedDict` keys and values are all invariant.

- [subtype_CheckedDict_key_covariant.py](conformance_suite/subtype_CheckedDict_key_covariant.py)
- [subtype_CheckedDict_key_contravariant.py](conformance_suite/subtype_CheckedDict_key_contravariant.py)
- [subtype_CheckedDict_value_covariant.py](conformance_suite/subtype_CheckedDict_value_covariant.py)
- [subtype_CheckedDict_value_contravariant.py](conformance_suite/subtype_CheckedDict_value_contravariant.py)

## The dynamic type

`dynamic` is compatible with any type

- [dynamic_as_int.py](conformance_suite/dynamic_as_int.py)
- [dynamic_as_callable.py](conformance_suite/dynamic_as_callable.py)
- [dynamic_as_CheckedDict.py](conformance_suite/dynamic_as_CheckedDict.py)
- [dynamic_as_user-defined_class.py](conformance_suite/dynamic_as_user-defined_class.py)

## Scope

Can't redelcare ordinary variable. 
TODO: some tests might fail in the current SP but these behavior will be implemented. See https://github.com/facebookincubator/cinder/issues/53

- [redeclare_var_with_def.py](conformance_suite/redeclare_var_with_def.py)
- [redeclare_var_with_class.py](conformance_suite/redeclare_var_with_class.py)
- [redeclare_var_with_var_same_type.py](conformance_suite/redeclare_var_with_var_same_type.py)
- [redeclare_var_with_var_dyn_to_type.py](conformance_suite/redeclare_var_with_var_dyn_to_type.py)
- [redeclare_var_flatten_if.py](conformance_suite/redeclare_var_flatten_if.py)