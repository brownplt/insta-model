# Reason: Test hitted a banned word int64
def test_array_create(self):
    codestr = """
        from __static__ import int64, Array
        def test() -> Array[int64]:
            x: Array[int64] = Array[int64]([1, 3, 5])
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertEqual(test(), array("l", [1, 3, 5]))
