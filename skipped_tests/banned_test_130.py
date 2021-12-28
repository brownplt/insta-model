# Reason: Test hitted a banned word int8
def test_primitive_iter(self) -> None:
    codestr = """
        from __static__ import int8
        def f():
            x: int8 = 42
            for a in x:
                pass
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot iterate over int8"):
        self.compile(codestr)
