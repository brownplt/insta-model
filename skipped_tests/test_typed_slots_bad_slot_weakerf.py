# Reason: Test hitted some skipped words
def test_typed_slots_bad_slot_weakerf(self):
    with self.assertRaises(TypeError):
        class C:
            __slots__ = ("__weakref__",)
            __slot_types__ = {"__weakref__": "object"}
