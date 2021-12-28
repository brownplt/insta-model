# Reason: Test hitted a banned word int64
def test_error_return_int(self):
    with self.assertRaisesRegex(TypedSyntaxError, bad_ret_type("int64", "dynamic")):
        code = self.compile(
            """
            from __static__ import ssize_t
            def f():
                y: ssize_t = 1
                return y
            """
        )
