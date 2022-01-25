# Static Semantics

This file describes the static semantics (type system) of an idealized Static Python.

## Empty Program

The empty program should type check.

- [empty_program.py](conformance_suite/empty_program.py)

## Base Types

`object` is inhabitable.

- [object_is_inhabitable.py](conformance_suite/object_is_inhabitable.py)

`None` is inhabitable.

- [None_is_inhabitable.py](conformance_suite/None_is_inhabitable.py)

`bool` is inhabitable.

- [bool_is_inhabitable.py](conformance_suite/bool_is_inhabitable.py)

`int` is inhabitable.

- [int_is_inhabitable.py](conformance_suite/int_is_inhabitable.py)

`str` is inhabitable.

- [str_is_inhabitable.py](conformance_suite/str_is_inhabitable.py)

`Exception` is inhabitable.

- [Exception_is_inhabitable.py](conformance_suite/Exception_is_inhabitable.py)

`list` is inhabitable.

- [list_is_inhabitable.py](conformance_suite/list_is_inhabitable.py)

`tuple` is inhabitable.

- [tuple_is_inhabitable.py](conformance_suite/tuple_is_inhabitable.py)

## Optional

`Optional[T]` is inhabitable by `None` and instances of `T`, but not others.

- [optional_is_inhabitable_none.py](conformance_suite/optional_is_inhabitable_none.py)
- [optional_is_inhabitable_nonnone.py](conformance_suite/optional_is_inhabitable_nonnone.py)
- [optional_is_inhabitable_other.py](conformance_suite/optional_is_inhabitable_other.py)

`Optional[T]` can be refined by a variety of language constructs.

- [optional_refine_if.py](conformance_suite/optional_refine_if.py)
- [optional_refine_is_None.py](conformance_suite/optional_refine_is_None.py)
- [optional_refine_or.py](conformance_suite/optional_refine_or.py)
- [optional_refine_and.py](conformance_suite/optional_refine_and.py)

## Union

`Optional`-liked union types are supported.

- [union_optional_is_supported_pos.py](conformance_suite/union_optional_is_supported_pos.py)
- [union_optional_is_supported_neg.py](conformance_suite/union_optional_is_supported_neg.py)

General union types fall back to `dynamic`.

- [union_other_is_dyn.py](conformance_suite/union_other_is_dyn.py)

## PyDict

`PyDict` is inhabitable.

- [PyDict_is_inhabitable.py](conformance_suite/PyDict_is_inhabitable.py)

Inserting `PyDict` entries is allowed statically.

- [PyDict_insert.py](conformance_suite/PyDict_insert.py)

Updating `PyDict` entries is allowed statically.

- [PyDict_update.py](conformance_suite/PyDict_update.py)

Deleting `PyDict` entries is allowed statically.

- [PyDict_delete_good_key.py](conformance_suite/PyDict_delete_good_key.py)
- [PyDict_delete_bad_key.py](conformance_suite/PyDict_delete_bad_key.py)

Looking up `PyDict` entries is allowed statically.

- [PyDict_lookup_good_key.py](conformance_suite/PyDict_lookup_good_key.py)
- [PyDict_lookup_bad_key.py](conformance_suite/PyDict_lookup_bad_key.py)

## CheckedDict[T1, T2]

`CheckedDict` must be constructed from `dict`.

- [CheckedDict_from_nondict.py](conformance_suite/CheckedDict_from_nondict.py)
- [CheckedDict_from_bad_dict.py](conformance_suite/CheckedDict_from_bad_dict.py)
- [CheckedDict_from_good_dict.py](conformance_suite/CheckedDict_from_good_dict.py)

Looking up `CheckedDict[T1, T2]` expects a `T1` key.

- [CheckedDict_lookup_good_key.py](conformance_suite/CheckedDict_lookup_good_key.py)
- [CheckedDict_lookup_bad_key.py](conformance_suite/CheckedDict_lookup_bad_key.py)
- [CheckedDict_lookup_dom.py](conformance_suite/CheckedDict_lookup_dom.py)

Looking up `CheckedDict[T1, T2]` returns a `T2` value.

- [CheckedDict_lookup_cod.py](conformance_suite/CheckedDict_lookup_cod.py)

Updating a `CheckedDict[T1, T2]` expects a `T1` key and a `T2` value.

- [CheckedDict_update.py](conformance_suite/CheckedDict_update.py)
- [CheckedDict_update_dom_key.py](conformance_suite/CheckedDict_update_dom_key.py)
- [CheckedDict_update_dom_val.py](conformance_suite/CheckedDict_update_dom_val.py)

Deleting `CheckedDict[T1, T2]` expects a `T1` key.

- [CheckedDict_delete_good_key.py](conformance_suite/CheckedDict_delete_good_key.py)
- [CheckedDict_delete_bad_key.py](conformance_suite/CheckedDict_delete_bad_key.py)
- [CheckedDict_delete_dom.py](conformance_suite/CheckedDict_delete_dom.py)

The key type of CheckedDicts can be `Optional[T]`.

- [CheckedDict_key_can_be_Optional.py](conformance_suite/CheckedDict_key_can_be_Optional.py)
- [CheckedDict_val_can_be_Optional.py](conformance_suite/CheckedDict_val_can_be_Optional.py)

`CheckedDict`-typed variables can be initialized with `dict` literals, in which case the keys and values will be checked statically.

- [CheckedDict_from_dict_literal_pos.py](conformance_suite/CheckedDict_from_dict_literal_pos.py)
- [CheckedDict_from_dict_literal_neg.py](conformance_suite/CheckedDict_from_dict_literal_neg.py)

## The dynamic type

`dynamic` is compatible with any type

- [dynamic_as_int.py](conformance_suite/dynamic_as_int.py)
- [dynamic_as_callable.py](conformance_suite/dynamic_as_callable.py)
- [dynamic_as_CheckedDict.py](conformance_suite/dynamic_as_CheckedDict.py)
- [dynamic_as_user-defined_class.py](conformance_suite/dynamic_as_user-defined_class.py)

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

### Class Variables (class-level fields)

Class variables might or might not be initialized at declaration.

- [class_variables_declare_only.py](conformance_suite/class_variables_declare_only.py)
- [class_variables_declare_and_init.py](conformance_suite/class_variables_declare_and_init.py)

Class variables can be redeclared in subclasses. But the new class variable must be of the same type.

- [class_variables_redeclare_in_subclass_same_type.py](conformance_suite/class_variables_redeclare_in_subclass_same_type.py)
- [class_variables_redeclare_in_subclass_sub_type.py](conformance_suite/class_variables_redeclare_in_subclass_sub_type.py)
- [class_variables_redeclare_in_subclass_sup_type.py](conformance_suite/class_variables_redeclare_in_subclass_sup_type.py)
- [class_variables_redeclare_in_subclass_less_precise_type.py](conformance_suite/class_variables_redeclare_in_subclass_less_precise_type.py)
- [class_variables_redeclare_in_subclass_more_precise_type.py](conformance_suite/class_variables_redeclare_in_subclass_more_precise_type.py)

Class variables are read-only at instance level.

- [class_variables_readable_at_instance_level.py](conformance_suite/class_variables_readable_at_instance_level.py)
- [class_variables_nonwritable_at_instance_level.py](conformance_suite/class_variables_nonwritable_at_instance_level.py)

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

### Fields (instance-level fields)

Attributes can not be initialized at class levels.

- [instance_variables_initialize_at_class.py](conformance_suite/instance_variables_initialize_at_class.py)

Overriding a field is a static error.

- [override_instance_field.py](conformance_suite/override_instance_field.py)

### Interaction between ClassVar (including methods) and fields

Overriding a method with a field is a static error.

- [override_instance_method_with_field.py](conformance_suite/override_instance_method_with_field.py)

Overriding a field with a method is a static error.

- [override_instance_field_with_method.py](conformance_suite/override_instance_field_with_method.py)

Class variables must not be shadowed by instance variables.

- [class_variables_shadow_by_instance_variables_same_class.py](conformance_suite/class_variables_shadow_by_instance_variables_same_class.py)
- [class_variables_shadow_by_instance_variables_sub_class.py](conformance_suite/class_variables_shadow_by_instance_variables_sub_class.py)

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

## Scope

Can't redelcare ordinary variables.

- [redeclare_var_with_def.py](conformance_suite/redeclare_var_with_def.py)
- [redeclare_var_with_class.py](conformance_suite/redeclare_var_with_class.py)
- [redeclare_var_with_var_same_type.py](conformance_suite/redeclare_var_with_var_same_type.py)
- [redeclare_var_with_var_dyn_to_type.py](conformance_suite/redeclare_var_with_var_dyn_to_type.py)
- [redeclare_var_flatten_if.py](conformance_suite/redeclare_var_flatten_if.py)
