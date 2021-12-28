# Reason: Test hitted a banned word int32
def test_typed_slots_bad_inst(self):
    class C:
        __slots__ = ("a",)
        __slot_types__ = {"a": ("__static__", "int32")}
    class D:
        pass
    with self.assertRaises(TypeError):
        C.a.__get__(D(), D)
