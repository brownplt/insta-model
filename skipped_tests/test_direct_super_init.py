# Reason: Format too complicated
def test_direct_super_init(self):
    value = 42
    expected = value
    codestr = f"""
        class Obj:
            pass
        class C:
            def __init__(self, x: Obj):
                pass
        class D:
            def __init__(self):
                C.__init__(None)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"None received for positional arg 'self', expected foo.C",
    ):
        self.compile(codestr, modname="foo")
