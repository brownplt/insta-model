# override_instance_method_covariant_output_pos.py

class C: pass
class D(C): pass

class A:
    def m(self) -> C:
        return C()

class B:
    def m(self) -> D:
        return D()

# This should pass.