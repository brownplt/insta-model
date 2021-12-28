# Reason: Test hitted a banned word int8
def test_chained_primitive_to_non_primitive(self):
    codestr = """
        from __static__ import int8
        def f():
            x: object
            y: int8 = 42
            x = y = 42
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Literal\\[42\\] cannot be assigned to object"
    ):
        self.compile(codestr, modname="foo")
