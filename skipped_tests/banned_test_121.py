# Reason: Test hitted a banned word int64
def test_error_nested_prim_decorator(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 0
            @x
            def g():
                pass
            return g
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "decorator cannot be a primitive"
    ):
        self.compile(code)
