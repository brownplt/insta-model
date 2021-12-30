# override_instance_method_covariant_output_pos.py
# This should pass.
# This should terminate.

class C: pass
class D(C): pass

class A:
    def m(self) -> C:
        return C()

class B(A):
    def m(self) -> D:
        return D()
