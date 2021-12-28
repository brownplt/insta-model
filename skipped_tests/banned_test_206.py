# Reason: Test hitted a banned word global
def test_closure_primitive(self):
    codestr = """
        from __static__ import int8
        def f():
            x: int8 = 0
            def g():
                return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot use primitives in global or closure scope"
    ):
        self.compile(codestr, modname="foo")
