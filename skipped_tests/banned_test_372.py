# Reason: Test hitted a banned word double
def test_array_len(self):
    codestr = """
        from __static__ import int64, char, double, Array
        from array import array
        def y():
            return len(Array[int64]([1, 3, 5]))
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    self.assertInBytecode(y, "FAST_LEN", FAST_LEN_ARRAY)
    with self.in_module(codestr) as mod:
        y = mod.y
        self.assertEqual(y(), 3)
