def test_clen_bad_arg(self):
    codestr = """
        from __static__ import clen
        def f(l):
            clen(l)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "bad argument type 'dynamic' for clen()"
    ):
        self.compile(codestr)
