# Reason: Can't be translated by any of the three translator
def test_assign_try_except_redeclare(self):
    codestr = """
        def testfunc():
            e: int
            try:
                pass
            except Exception as e:
                pass
            return 42
    """
    code = self.compile(codestr, modname="foo")
