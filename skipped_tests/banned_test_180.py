# Reason: Test hitted a banned word int8
def test_list_get_primitive_int(self):
    codestr = """
        from __static__ import int8
        def f():
            l = [1, 2, 3]
            x: int8 = 1
            return l[x]
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "SEQUENCE_GET", SEQ_LIST)
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), 2)
