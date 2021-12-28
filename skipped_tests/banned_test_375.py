# Reason: Test hitted a banned word int64
def test_array_not_subclassable(self):
    with self.assertRaises(TypeError):
        class C(Array[int64]):
            pass
    with self.assertRaises(TypeError):
        class C(Array):
            pass
