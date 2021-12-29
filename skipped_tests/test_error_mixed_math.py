# Reason: Can't be translated by any of the three translator
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
