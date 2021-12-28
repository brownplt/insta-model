# Reason: Test hitted a banned word int8
def test_extremum_primitive(self):
    codestr = """
        from __static__ import int8
        def f() -> None:
            a: int8 = 4
            b: int8 = 5
            min(a, b)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Call argument cannot be a primitive"
    ):
        self.compile(codestr, modname="foo.py")
