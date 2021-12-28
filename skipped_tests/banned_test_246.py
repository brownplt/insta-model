# Reason: Test hitted a banned word global
def test_module_primitive(self):
    codestr = """
        from __static__ import int8
        x: int8
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot use primitives in global or closure scope"
    ):
        self.compile(codestr, modname="foo")
