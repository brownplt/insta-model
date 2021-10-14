# Static Semantics

This file describes the static semantics (type system) of the idealized Static Python.

## Base Types

`bool` is inhabitable.

- [bool_is_inhabitable.py](conformance_suite/bool_is_inhabitable.py)

`int` is inhabitable.

- [int_is_inhabitable.py](conformance_suite/int_is_inhabitable.py)

`str` is inhabitable.

- [str_is_inhabitable.py](conformance_suite/str_is_inhabitable.py)

## PyDict

`PyDict` is inhabitable.

- [PyDict_is_inhabitable.py](conformance_suite/PyDict_is_inhabitable.py)

Inserting `PyDict` entries is allowed.

- [PyDict_insert.py](conformance_suite/PyDict_insert.py)

Updating `PyDict` entries is allowed.

- [PyDict_update.py](conformance_suite/PyDict_update.py)

Deleting `PyDict` entries is allowed.

- [PyDict_delete.py](conformance_suite/PyDict_delete.py)

Looking up `PyDict` entries with good keys is allowed.

- [PyDict_lookup_good_keys.py](conformance_suite/PyDict_lookup_good_keys.py)

Looking up `PyDict` entries with bad keys is allowed.

- [PyDict_lookup_bad_keys.py](conformance_suite/PyDict_lookup_bad_keys.py)

## CheckedDict[T0, T1]

`CheckedDict` must be constructed from `dict`.
- [CheckedDict_from_dict_neg.py](conformance_suite/CheckedDict_from_dict_neg.py)
- [CheckedDict_from_dict_pos.py](conformance_suite/CheckedDict_from_dict_pos.py)

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

## Classes

### Construct instances

`__init__` checks its arity.

- [init_checks_arity.py](conformance_suite/init_checks_arity.py)

`__init__` checks its argument types.

- [init_checks_type.py](conformance_suite/init_checks_type.py)

### Inheritance/subclassing

Inheriting builtin classes is allowed.

- [subclass_builtin.py](conformance_suite/subclass_builtin.py)

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

## Instances

Looking up a declared field is valid.
TODO

Looking up a declared parent field is valid.
TODO

Looking up an undeclared field is invalid.
TODO

TODO: field update

TODO: insert new field?

TODO: delete field?

TODO: method call

## Subtyping

`bool` is a subtype of `int`

- https://github.com/facebookincubator/cinder/issues/46
- [bool_is_a_subtype_of_int_neg.py](conformance_suite/bool_is_a_subtype_of_int_neg.py)
- [bool_is_a_subtype_of_int_pos.py](conformance_suite/bool_is_a_subtype_of_int_pos.py)

If `C` is a subclass of `D`, then `C` is a subtype of `D`.

- [child_is_a_subtype_of_parent_neg.py](conformance_suite/child_is_a_subtype_of_parent_neg.py)
- [child_is_a_subtype_of_parent_pos.py](conformance_suite/child_is_a_subtype_of_parent_pos.py)