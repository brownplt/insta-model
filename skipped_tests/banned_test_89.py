# Reason: Test hitted a banned word int64
def test_error_starred_primitive(self):
    code = """
        from __static__ import int64
        def g(*args):
            pass
        def f(a):
            x: int64 = 0
            return f(*x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "starred expression cannot be primitive"
    ):
        self.compile(code)
