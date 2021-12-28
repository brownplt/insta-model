# Reason: Test hitted a banned word int8
def test_vector_invalid_literal(self):
    codestr = f"""
        from __static__ import int8, Vector
        def test() -> Vector[int8]:
            x: Vector[int8] = Vector[int8]()
            x.append(128)
            return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Literal\[128\] received for positional arg 'v', expected int8",
    ):
        self.compile(codestr)
