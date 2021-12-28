# Reason: Test hitted a banned word int8
def test_primitive_args_typecall(self):
    codestr = """
        from __static__ import int8
        def n() -> int:
            x: int8 = 3
            return int(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Call argument cannot be a primitive"
    ):
        self.compile(codestr, modname="foo.py")
