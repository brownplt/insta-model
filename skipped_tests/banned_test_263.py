# Reason: Test hitted a banned word int64
def test_array_import(self):
    codestr = """
        from __static__ import int64, Array
        def test() -> Array[int64]:
            x: Array[int64] = Array[int64](1)
            x[0] = 1
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertEqual(test(), array("L", [1]))
