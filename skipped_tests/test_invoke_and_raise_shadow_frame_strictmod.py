# Reason: Hitted a skipped word (__static__.compiler_flags)
def test_invoke_and_raise_shadow_frame_strictmod(self):
    codestr = """
    from __static__.compiler_flags import shadow_frame
    def x():
        raise TypeError()
    def y():
        return x()
    """
    with self.in_strict_module(codestr) as mod:
        y = mod.y
        x = mod.x
        with self.assertRaises(TypeError):
            y()
        self.assert_jitted(x)
        self.assertInBytecode(
            y,
            "INVOKE_FUNCTION",
            ((mod.__name__, "x"), 0),
        )
