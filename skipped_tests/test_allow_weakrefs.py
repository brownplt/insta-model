# Reason: Test hitted some skipped words
def test_allow_weakrefs(self):
    codestr = """
        from __static__ import allow_weakrefs
        import weakref
        @allow_weakrefs
        class C:
            pass
        def f(c: C):
            return weakref.ref(c)
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        c = C()
        ref = mod.f(c)
        self.assertIs(ref(), c)
        del c
        self.assertIs(ref(), None)
        self.assertEqual(C.__slots__, ("__weakref__",))
