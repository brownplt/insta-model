def test_none_subscript(self):
    codestr = """
        def f():
            x = None
            return x[0]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "'NoneType' object is not subscriptable"
    ):
        self.compile(codestr, modname="foo")
