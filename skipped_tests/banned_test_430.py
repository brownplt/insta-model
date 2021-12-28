# Reason: Test hitted a banned word int64
def test_array_slice(self):
    v = Array[int64]([1, 2, 3, 4])
    self.assertEqual(v[1:3], Array[int64]([2, 3]))
    self.assertEqual(type(v[1:2]), Array[int64])
