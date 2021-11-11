# Reason: Test hitted some skipped words
def test_shadow_frame_generator(self):
    codestr = """
    from __static__.compiler_flags import shadow_frame
    def g():
        for i in range(10):
            yield i
    def f():
        return list(g())
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertTrue(f.__code__.co_flags & CO_SHADOW_FRAME)
        self.assertEqual(f(), list(range(10)))
        self.assert_jitted(f)
