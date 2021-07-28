# override_instance_method_with_field.py

class C:
    def x(self): 
        pass

class D(C):
    x: int

# This should error.