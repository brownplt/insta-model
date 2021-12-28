# Reason: Test hitted a banned word int8
def test_primitive_subscr(self) -> None:
    codestr = """
        from __static__ import int8
        def f():
            x: int8 = 42
            print(x[42])
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot index int8"):
        self.compile(codestr)
