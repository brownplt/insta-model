# child_is_a_subtype_of_parent_neg.py
# This should fail.

class C:
    pass

class D(C):
    pass

x: D = C()