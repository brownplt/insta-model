# Reason: Test hitted a banned word int64
def test_list_set_primitive_int_2(self):
    codestr = """
        from __static__ import int64
        def f(l1):
            l2 = [None] * len(l1)
            i: int64 = 0
            for item in l1:
                l2[i] = item + 1
                i += 1
            return l2
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "SEQUENCE_SET")
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f([1, 2000]), [2, 2001])
