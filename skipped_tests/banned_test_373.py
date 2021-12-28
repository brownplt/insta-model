# Reason: Test hitted a banned word int32
def test_array_isinstance(self):
    x = Array[int64](0)
    self.assertTrue(isinstance(x, Array[int64]))
    self.assertFalse(isinstance(x, Array[int32]))
    self.assertTrue(issubclass(Array[int64], Array[int64]))
    self.assertFalse(issubclass(Array[int64], Array[int32]))
