# Reason: Test hitted a banned word int64
def test_assign_prim_to_class(self):
    codestr = """
        from __static__ import int64
        class C: pass
        def f():
            x: C = C()
            y: int64 = 42
            x = y
    """
    with self.assertRaisesRegex(TypedSyntaxError, type_mismatch("int64", "foo.C")):
        self.compile(codestr, modname="foo")
