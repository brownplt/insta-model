# Reason: Test hitted a banned word Array
def test_array_call_unbound(self):
    codestr = """
        from __static__ import Array
        def f() -> Array:
            return Array([1, 2, 3])
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"create instances of a generic Type\[Exact\[Array\[T\]\]\]",
    ):
        self.compile(codestr, modname="foo")
