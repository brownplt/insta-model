# Reason: Hitted a skipped word (__setattr__)
def test_user_defined_class_setattr_undefined(self):
    codestr = """
    class F:
        hihello: str
    def fn():
        f = F()
        F.__setattr__(f, "hihello", "itsme")
        return f
    """
    with self.in_module(codestr, name="t3") as mod:
        fn = mod.fn
        self.assertInBytecode(fn, "LOAD_METHOD", "__setattr__")
        res = fn()
        self.assertEqual(res.hihello, "itsme")
