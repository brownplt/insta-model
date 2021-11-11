def test_typed_slots_bad_slots(self):
    with self.assertRaises(TypeError):
        class C:
            __slots__ = ("a",)
            __slot_types__ = None
