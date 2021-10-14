# child_is_a_subtype_of_parent_pos.py
# This should pass.

class C:
    pass

class D(C):
    pass

x: C = D()