# Reason: Test hitted a banned word test_assign_try_except_redeclare_unknown_type
def test_assign_try_except_redeclare_unknown_type(self):
    codestr = """
        def testfunc():
            e: int
            try:
                pass
            except UnknownException as e:
                pass
            return 42
    """
    code = self.compile(codestr, modname="foo")
