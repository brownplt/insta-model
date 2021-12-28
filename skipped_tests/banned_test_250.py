# Reason: Test hitted a banned word nonlocal
def test_nonlocal_primitive(self):
    codestr = """
        from __static__ import int8
        def f():
            x: int8 = 0
            def g():
                nonlocal x
                x = 1
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot use primitives in global or closure scope"
    ):
        self.compile(codestr, modname="foo")
