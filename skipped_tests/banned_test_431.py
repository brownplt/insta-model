# Reason: Test hitted a banned word int64
def test_vector_slice(self):
    v = Vector[int64]([1, 2, 3, 4])
    self.assertEqual(v[1:3], Vector[int64]([2, 3]))
    self.assertEqual(type(v[1:2]), Vector[int64])
