def test_generic_type_error(self):
    codestr = """
        from xxclassloader import spamobj
        def testfunc():
            x = spamobj[str]()
            x.setstate(42)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Literal\[42\] received for positional arg 1, expected str",
    ):
        code = self.compile(codestr, modname="foo")
