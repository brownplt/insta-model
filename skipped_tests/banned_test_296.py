# Reason: Test hitted a banned word int8
def test_primitive_args_nonstrict(self):
    codestr = """
        from __static__ import int8, int16, box
        def f(x: int8, y: int16) -> int16:
            return x + y
        def g() -> int:
            return box(f(1, 300))
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.g(), 301)
