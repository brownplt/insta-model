# Reason: Test hitted a banned word int64
def test_int_compare_to_cbool_reversed(self):
    codestr = """
        from __static__ import int64, cbool
        def foo(i: int64) -> cbool:
            return 0 == i
    """
    with self.in_module(codestr) as mod:
        foo = mod.foo
        self.assertEqual(foo(0), True)
        self.assertEqual(foo(1), False)
