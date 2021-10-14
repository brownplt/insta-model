# override_instance_method_with_field.py
# This should fail.

class C:
    def x(self): 
        pass

class D(C):
    x: int
