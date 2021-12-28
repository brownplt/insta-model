# Reason: Test hitted a banned word int64
def test_chained_assign_type_propagation_failure_redefine_2(self):
    codestr = """
        from __static__ import int64, char, Array
        def test2() -> Array[char]:
            x: Array[int64] = Array[int64]([54])
            y = x = Array[char]([48])
            return y
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        type_mismatch(
            "Exact[Array[char]]",
            "Exact[Array[int64]]",
        ),
    ):
        self.compile(codestr, modname="foo")
