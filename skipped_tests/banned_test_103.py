# Reason: Test hitted a banned word int64
def test_assert_primitive_msg(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 1
            assert False, x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "assert message cannot be a primitive"
    ):
        self.compile(code)
