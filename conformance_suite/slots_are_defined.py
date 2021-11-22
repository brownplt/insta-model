# slots_is_defined.py
# This should pass.
# This should terminate.

class C:
    pass

assert C.__slots__ == ()