# Reason: Test hitted a banned word double
def test_double_decl(self):
    codestr = """
        def f():
            x: int
            x: str
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot redefine local variable x"
    ):
        self.compile(codestr, modname="foo")
    codestr = """
        def f():
            x = 42
            x: str
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot redefine local variable x"
    ):
        self.compile(codestr, modname="foo")
    codestr = """
        def f():
            x = 42
            x: int
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot redefine local variable x"
    ):
        self.compile(codestr, modname="foo")
