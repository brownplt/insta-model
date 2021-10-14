# override_instance_method_covariant_output_neg.py
# This should fail.

class C: pass
class D(C): pass

class A:
    def m(self) -> D:
        return C()

class B(A):
    def m(self) -> C:
        return C()
