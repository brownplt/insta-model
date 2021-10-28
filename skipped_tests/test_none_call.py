def test_none_call(self):
    codestr = """
        def f():
            x = None
            return x()
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "'NoneType' object is not callable"
    ):
        self.compile(codestr, modname="foo")
