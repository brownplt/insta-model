# Reason: Test hitted a banned word int64
def test_vector_assign_non_primitive(self):
    codestr = """
        from __static__ import int64, Vector
        def test(abc) -> Vector[int64]:
            x: Vector[int64] = Vector[int64](2)
            i: int64 = 0
            x[i] = abc
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("dynamic", "int64")
    ):
        self.compile(codestr)
