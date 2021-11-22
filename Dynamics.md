# Dynamic Semantics

This file describes the dynamic semantics (runtime) of the idealized Static Python.

## Empty Program

The empty program should type check and terminate.

- [empty_program.py](conformance_suite/empty_program.py)

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

Inserting `CheckedDict` entries is allowed.

- [CheckedDict_insert.py](conformance_suite/CheckedDict_insert.py)

Updating `CheckedDict` entries is allowed.

- [CheckedDict_update_key_pos.py](conformance_suite/CheckedDict_update_key_pos.py)
- [CheckedDict_update_val_pos.py](conformance_suite/CheckedDict_update_val_pos.py)

Deleting `CheckedDict` entries with good keys is allowed.

- [CheckedDict_delete_pos.py](conformance_suite/CheckedDict_delete_pos.py)

Looking up `CheckedDict` entries with good keys is allowed.

- [CheckedDict_lookup_key_pos.py](conformance_suite/CheckedDict_lookup_key_pos.py)

After deletion, looking up the key should fail.

- [CheckedDict_delete_then_lookup.py](conformance_suite/CheckedDict_delete_then_lookup.py)

After insertion, looking up the key should succeed.

- [CheckedDict_insert_then_lookup.py](conformance_suite/CheckedDict_insert_then_lookup.py)

After updating, looking up the key should give the new value.

- [CheckedDict_update_then_lookup.py](conformance_suite/CheckedDict_update_then_lookup.py)

CheckedDicts check inputs.

- [CheckedDict_from_nondict.py](conformance_suite/CheckedDict_from_nondict.py)
- [CheckedDict_from_bad_dict.py](conformance_suite/CheckedDict_from_bad_dict.py)

CheckedDicts check keys.

- [CheckedDict_lookup_checks_keys.py](conformance_suite/CheckedDict_lookup_checks_keys.py)
- [CheckedDict_update_checks_keys.py](conformance_suite/CheckedDict_update_checks_keys.py)
- [CheckedDict_delete_checks_keys.py](conformance_suite/CheckedDict_delete_checks_keys.py)

CheckedDicts check new values.

- [CheckedDict_update_checks_values.py](conformance_suite/CheckedDict_update_checks_values.py)

## Procedure

Procedures work.

- [procedure_works.py](conformance_suite/procedure_works.py)

Procedures check arity.

- [procedure_check_arity_dynamically.py](conformance_suite/procedure_check_arity_dynamically.py)

Procedures check argument types.

- [procedure_check_argument_type_dynamically.py](conformance_suite/procedure_check_argument_type_dynamically.py)

Procedures check return types.

- [procedure_check_return_type_dynamically.py](conformance_suite/procedure_check_return_type_dynamically.py)

## Optional types

TODO

## Optimizations

TODO: CheckedDict methods

TODO: Other optimizations

## Runtime Checks

Casting to a super class is okay.

- [upcast_bool_to_int.py](conformance_suite/upcast_bool_to_int.py)

Casting to a sub class may or maynot be okay

- [downcast_int_to_bool_pos.py](conformance_suite/downcast_int_to_bool_pos.py)
- [downcast_int_to_bool_neg.py](conformance_suite/downcast_int_to_bool_neg.py)

TODO: cast PyDict to CheckedDict and the other way

## Slots

- [slots_are_defined.py](conformance_suite/slots_are_defined.py)
- [slots_contain_object_fields_but_not_class_fields.py](conformance_suite/slots_contain_object_fields_but_not_class_fields.py)
- [slots_do_not_contain_methods.py](conformance_suite/slots_do_not_contain_methods.py)
- [slots_do_not_accumulate.py](conformance_suite/slots_do_not_accumulate.py)

## Attributes / fields

- [partially_static_class_update_dynamic_field.py](conformance_suite/partially_static_class_update_dynamic_field.py)
- [static_class_update_dynamic_field.py](conformance_suite/static_class_update_dynamic_field.py)
- [static_class_update_static_field.py](conformance_suite/static_class_update_static_field.py)
