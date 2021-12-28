# Reason: Test hitted a banned word int64
def test_generic_type_box_box(self):
    codestr = """
        from xxclassloader import spamobj
        def testfunc():
            x = spamobj[str]()
            return (x.getint(), )
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("int64", "dynamic")
    ):
        code = self.compile(codestr, modname="foo")
