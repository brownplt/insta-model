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

## CheckedDict

`CheckedDict` is inhabitable.

- [CheckedDict_is_inhabitable.py](conformance_suite/CheckedDict_is_inhabitable.py)

Inserting `CheckedDict` entries is allowed.

- [CheckedDict_insert.py](conformance_suite/CheckedDict_insert.py)

Updating `CheckedDict` entries is allowed.

- [CheckedDict_update.py](conformance_suite/CheckedDict_update.py)

Deleting `CheckedDict` entries with good keys is allowed.

- [CheckedDict_delete_good_key.py](conformance_suite/CheckedDict_delete_good_key.py)

Deleting `CheckedDict` entries with bad keys is allowed at compile time, but should fail at runtime.

- [CheckedDict_delete_bad_key.py](conformance_suite/CheckedDict_delete_bad_key.py)

Looking up `CheckedDict` entries with good keys is allowed.

- [CheckedDict_lookup_good_key.py](conformance_suite/CheckedDict_lookup_good_key.py)

Looking up `CheckedDict` entries with bad keys is allowed at compile time, but should fail at runtime.

- [CheckedDict_lookup_bad_key.py](conformance_suite/CheckedDict_lookup_bad_key.py)

After deletion, looking up the key should fail.

- [CheckedDict_delete_then_lookup.py](conformance_suite/CheckedDict_delete_then_lookup.py)

After insertion, looking up the key should succeed.

- [CheckedDict_insert_then_lookup.py](conformance_suite/CheckedDict_insert_then_lookup.py)

After updating, looking up the key should give the new value.

TODO: CheckedDict checks everything


## Runtime Checks

Casting to a super class is okay.

- [upcast_bool_to_int.py](conformance_suite/upcast_bool_to_int.py)
- [upcast_int_to_float.py](conformance_suite/upcast_int_to_float.py)
- [upcast_bool_to_float.py](conformance_suite/upcast_bool_to_float.py)

Casting to a sub class may or maynot be okay

- [downcast_float_to_int_pos.py](conformance_suite/downcast_float_to_int_pos.py)
- [downcast_float_to_int_neg.py](conformance_suite/downcast_float_to_int_neg.py)

TODO: cast PyDict to CheckedDict and the other way