def test_generic_optional_type_param_error(self):
    codestr = """
        from xxclassloader import spamobj
        def testfunc():
            x = spamobj[str]()
            x.setstateoptional(42)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Literal\[42\] received for positional arg 1, expected Optional\[str\]",
    ):
        code = self.compile(codestr, modname="foo")
