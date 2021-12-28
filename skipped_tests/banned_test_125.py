# Reason: Test hitted a banned word int8
def test_primitive_call(self) -> None:
    codestr = """
        from __static__ import int8
        def f():
            x: int8 = 42
            print(x())
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot call int8"):
        self.compile(codestr)
