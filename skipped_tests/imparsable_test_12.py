# Reason: Format too complicated
def test_error_mixed_math(self):
    with self.assertRaises(TypedSyntaxError):
        code = self.compile(
            """
            from __static__ import ssize_t
            def f():
                y = 1
                x: ssize_t = 42 + y
            """
        )
