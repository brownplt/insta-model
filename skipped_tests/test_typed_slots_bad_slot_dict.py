# Reason: Hitted a skipped word (__slots__)
def test_typed_slots_bad_slot_dict(self):
    with self.assertRaises(TypeError):
        class C:
            __slots__ = ("__dict__",)
            __slot_types__ = {"__dict__": "object"}
