# Reason: Test hitted a banned word int8
def test_primitive_out_of_range(self):
    codestr = f"""
        from __static__ import int8, box
        def f() -> int:
            x = int8(255)
            return box(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "type mismatch: Literal\\[255\\] cannot be assigned to int8",
    ):
        self.compile(codestr)
