# Reason: Test hitted a banned word int8
def test_primitive_invoke(self) -> None:
    codestr = """
        from __static__ import int8
        def f():
            x: int8 = 42
            print(x.__str__())
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot load attribute from int8"
    ):
        self.compile(codestr)
