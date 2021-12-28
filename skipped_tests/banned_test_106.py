# Reason: Test hitted a banned word int64
def test_dict_primitive(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 1
            return {x: 42}
    """
    with self.assertRaisesRegex(TypedSyntaxError, "dict keys cannot be primitives"):
        self.compile(code)
    code = """
        from __static__ import int64
        def f():
            x: int64 = 1
            return {42: x}
    """
    with self.assertRaisesRegex(TypedSyntaxError, "dict keys cannot be primitives"):
        self.compile(code)
    code = """
        from __static__ import int64
        def f():
            x: int64 = 1
            return {**x}
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "dict splat cannot be a primitive"
    ):
        self.compile(code)
