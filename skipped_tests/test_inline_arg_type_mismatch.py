def test_inline_arg_type_mismatch(self):
    codestr = """
        from __static__ import inline
        @inline
        def f(x: int) -> bool:
            return x == 1
        def g(arg: str) -> bool:
            return f(arg)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, r"str received for positional arg 'x', expected int"
    ):
        self.compile(codestr)
