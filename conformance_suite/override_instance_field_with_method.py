# override_instance_field_with_method.py

# Borrowed from `test_static.py`.
# The original name is `test_incompat_override.py`.

class C:
    x: int

class D(C):
    def x(self): 
        pass

# This should error.