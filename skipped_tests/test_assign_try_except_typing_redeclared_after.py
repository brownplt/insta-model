# Reason: Can't be translated by any of the three translator
def test_assign_try_except_typing_redeclared_after(self):
    codestr = """
        def testfunc():
            try:
                pass
            except Exception as e:
                pass
            e: int = 42
            return 42
    """
    # We don't do anything special w/ Exception type yet, but it should compile
    code = self.compile(codestr, modname="foo")
