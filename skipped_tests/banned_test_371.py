# Reason: Test hitted a banned word int64
def test_vector_deepcopy(self):
    v = Vector[int64]([1, 2, 3, 4])
    self.assertEqual(v, deepcopy(v))
    self.assertIsNot(v, deepcopy(v))
    self.assertEqual(type(v), type(deepcopy(v)))
