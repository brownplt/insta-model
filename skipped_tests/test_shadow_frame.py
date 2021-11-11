# Reason: Test hitted some skipped words
def test_shadow_frame(self):
    codestr = """
    from __static__.compiler_flags import shadow_frame
    def f():
        return 456
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertTrue(f.__code__.co_flags & CO_SHADOW_FRAME)
        self.assertEqual(f(), 456)
        self.assert_jitted(f)
