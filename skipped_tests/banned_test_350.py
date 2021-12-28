# Reason: Test hitted a banned word int8
def test_primitive_args_funccall_int(self):
    codestr = """
        from __static__ import int8
        def f(foo: int):
            pass
        def n() -> int:
            x: int8 = 3
            return f(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"int8 received for positional arg 'foo', expected int",
    ):
        self.compile(codestr, modname="foo.py")
