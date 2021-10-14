# override_instance_field_with_method.py
# This should fail.

# This program is adapted from the `test_incompat_override` test in `test_static.py`.

class C:
    x: int

class D(C):
    def x(self): 
        pass
