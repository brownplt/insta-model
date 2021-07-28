# override_instance_method_covariant_output_neg.py

class C: pass
class D(C): pass

class A:
    def m(self) -> D:
        return C()

class B:
    def m(self) -> C:
        return C()

# This should fail.