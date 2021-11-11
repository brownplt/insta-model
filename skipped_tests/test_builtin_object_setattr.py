# Reason: Test hitted some skipped words
def test_builtin_object_setattr(self):
    codestr = """
    class C:
        a: int
        def fn(self):
            object.__setattr__(self, "a", 1)
    """
    with self.in_module(codestr, name="t1") as mod:
        C = mod.C
        self.assertInBytecode(C.fn, "LOAD_METHOD", "__setattr__")
        c = C()
        c.fn()
        self.assertEqual(c.a, 1)
