# Reason: Test hitted a banned word global
def test_implicit_module_primitive(self):
    codestr = """
        from __static__ import int8
        x = y = int8(0)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot use primitives in global or closure scope"
    ):
        self.compile(codestr, modname="foo")
