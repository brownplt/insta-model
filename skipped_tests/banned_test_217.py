# Reason: Test hitted a banned word int8
def test_vector_wrong_size(self):
    codestr = f"""
        from __static__ import int8, int16, Vector
        def test() -> Vector[int8]:
            y: int16 = 1
            x: Vector[int8] = Vector[int8]()
            x.append(y)
            return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"int16 received for positional arg 'v', expected int8",
    ):
        self.compile(codestr)
