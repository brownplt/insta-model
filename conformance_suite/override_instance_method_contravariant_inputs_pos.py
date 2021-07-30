# override_instance_method_contravariant_inputs_pos.py
# This should pass.

class C: pass
class D(C): pass

class A:
    def m(self, x: D) -> None:
        return

class B(A):
    def m(self, x: C) -> None:
        return
