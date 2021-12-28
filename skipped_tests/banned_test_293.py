# Reason: Test hitted a banned word xxclassloader
def test_spamobj_no_params(self):
    codestr = """
        from xxclassloader import spamobj
        def f():
            x = spamobj()
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"cannot create instances of a generic Type\[xxclassloader.spamobj\[T\]\]",
    ):
        self.compile(codestr, modname="foo")