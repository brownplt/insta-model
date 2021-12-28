# Reason: Test hitted a banned word int8
def test_list_set_primitive_int(self):
    codestr = """
        from __static__ import int8
        def f():
            l = [1, 2, 3]
            x: int8 = 1
            l[x] = 5
            return l
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "SEQUENCE_SET")
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), [1, 5, 3])
