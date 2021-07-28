# override_instance_method_contravariant_inputs_pos.py

class C: pass
class D(C): pass

class A:
    def m(self, x: D) -> None:
        return

class B(A):
    def m(self, x: C) -> None:
        return

# This should pass.