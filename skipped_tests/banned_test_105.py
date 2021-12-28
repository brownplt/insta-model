# Reason: Test hitted a banned word int64
def test_assert_primitive(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 1
            assert x
    """
    with self.in_module(code) as mod:
        f = mod.f
        self.assertInBytecode(f, "POP_JUMP_IF_NONZERO")
