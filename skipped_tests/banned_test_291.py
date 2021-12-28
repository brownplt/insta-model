# Reason: Test hitted a banned word int8
def test_primitive_args_funcdef_missing_kw_call(self):
    codestr = """
        from __static__ import int8, box
        def testfunc(x: int8, foo):
            return box(x), foo
    """
    with self.in_strict_module(codestr) as mod:
        self.assertEqual(mod.testfunc(-128, foo=42), (-128, 42))
