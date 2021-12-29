# Reason: Hitted a skipped word (xxclassloader)
def test_generic_optional_type_param_2(self):
    codestr = """
        from xxclassloader import spamobj
        def testfunc():
            x = spamobj[str]()
            x.setstateoptional('abc')
    """
    code = self.compile(codestr, modname="foo")
