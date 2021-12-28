# Reason: Test hitted a banned word int64
def test_mixed_binop(self):
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot add int64 and Literal\\[1\\]"
    ):
        self.bind_module(
            """
            from __static__ import ssize_t
            def f():
                x: ssize_t = 1
                y = 1
                x + y
        """
        )
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot add Literal\\[1\\] and int64"
    ):
        self.bind_module(
            """
            from __static__ import ssize_t
            def f():
                x: ssize_t = 1
                y = 1
                y + x
        """
        )
