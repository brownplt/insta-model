# Reason: Test hitted a banned word int64
def test_error_nested_annass_prim_annotation(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 0
            y: x = 2
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "annotation can not be a primitive value"
    ):
        self.compile(code)
