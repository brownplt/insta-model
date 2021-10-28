def test_generic_optional_type_param(self):
    codestr = """
        from xxclassloader import spamobj
        def testfunc():
            x = spamobj[str]()
            x.setstateoptional(None)
    """
    code = self.compile(codestr, modname="foo")
