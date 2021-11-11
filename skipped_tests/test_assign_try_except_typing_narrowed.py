# Reason: Code hitted some skipped words
def test_assign_try_except_typing_narrowed(self):
    codestr = """
        class E(Exception):
            pass
        def testfunc():
            e: Exception
            try:
                pass
            except E as e:
                pass
            return 42
    """
    # We don't do anything special w/ Exception type yet, but it should compile
    code = self.compile(codestr, modname="foo")
