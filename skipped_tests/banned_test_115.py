# Reason: Test hitted a banned word int64
def test_subscr_primitive(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 0
            return [*x]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot use primitive in starred expression"
    ):
        self.compile(code)
