# Static Semantics

This file describes the static semantics (type system) of the idealized Static Python.

## Classes

### Construct instances

Class constructors (`__init__` methods) check their arity.

* [init_checks_arity.py](./conformance_suite/init_checks_arity.py)

Class constructors (`__init__` methods) check their argument types.

* [init_checks_type.py](./conformance_suite/init_checks_type.py)

### Inheritance

Overriding a method with a field is a static error.

* [override_method_with_field.py](./conformance_suite/override_method_with_field.py)

Overriding a field with a method is a static error.

* [override_field_with_method.py](./conformance_suite/override_field_with_method.py)

## Subtyping

`bool` is a subtype of `int`

* https://github.com/facebookincubator/cinder/issues/46

Subclassing implies subtyping.

* TODO