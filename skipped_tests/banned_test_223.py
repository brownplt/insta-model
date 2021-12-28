# Reason: Test hitted a banned word int64
def test_array_assign_wrong_type(self):
    codestr = """
        from __static__ import int64, char, Array
        def test() -> None:
            x: Array[int64] = Array[char]([48])
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        type_mismatch(
            "Exact[Array[char]]",
            "Exact[Array[int64]]",
        ),
    ):
        self.compile(codestr, modname="foo")
