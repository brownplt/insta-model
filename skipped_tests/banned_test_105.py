# Reason: Test hitted a banned word int64
def test_list_slice_primitive(self):
    code = """
        from __static__ import int64
        def f():
            x = [2,3,4]
            y: int64 = 1
            return x[y:2]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "slice indices cannot be primitives"
    ):
        self.compile(code)
    code = """
        from __static__ import int64
        def f():
            x = [2,3,4]
            y: int64 = 1
            return x[0:y]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "slice indices cannot be primitives"
    ):
        self.compile(code)
    code = """
        from __static__ import int64
        def f():
            x = [2,3,4]
            y: int64 = 1
            return x[0:2:y]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "slice indices cannot be primitives"
    ):
        self.compile(code)
