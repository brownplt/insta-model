# Reason: Can't be translated by any of the three translator
def test_if_optional_reassign(self):
    codestr = """
    class C: pass
    def testfunc(abc: Optional[C]):
        if abc is not None:
            abc = None
    """
    code = self.compile(codestr, modname="foo")
