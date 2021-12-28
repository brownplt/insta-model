# Reason: Test hitted a banned word int32
def test_bad_slots_qualname_conflict(self):
    with self.assertRaises(ValueError):
        class C:
            __slots__ = ("x",)
            __slot_types__ = {"x": ("__static__", "int32")}
            x = 42
