# Reason: Test hitted a banned word int8
def test_list_append(self):
    codestr = """
        from __static__ import int8
        def f():
            l = [1, 2, 3]
            l.append(4)
            return l
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "LIST_APPEND", 1)
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), [1, 2, 3, 4])
