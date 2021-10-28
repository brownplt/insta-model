def test_unknown_imported_annotation(self):
    codestr = """
        from unknown_mod import foo
        def testfunc():
            x: foo = 42
            return x
    """
    code = self.compile(codestr, modname="foo")
