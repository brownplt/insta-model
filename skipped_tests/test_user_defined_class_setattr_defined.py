def test_user_defined_class_setattr_defined(self):
    codestr = """
    class E:
        hihello: str
        def __setattr__(self, key: str, val: object):
            object.__setattr__(self, key + "hello", val)
    def fn():
        e = E()
        E.__setattr__(e, "hi", "itsme")
        return e
    """
    with self.in_module(codestr, name="t2") as mod:
        fn = mod.fn
        self.assertInBytecode(
            fn, "INVOKE_FUNCTION", (("t2", "E", "__setattr__"), 3)
        )
        res = fn()
        self.assertEqual(res.hihello, "itsme")
