# Reason: Format too complicated
def test_unknown_annotation(self):
    codestr = """
        def f(a):
            x: foo = a
            return x.bar
    """
    code = self.compile(codestr, modname="foo")
    class C:
        bar = 42
    f = self.run_code(codestr)["f"]
    self.assertEqual(f(C()), 42)
