# Reason: Test hitted a banned word box
def test_inferred_primitive_type(self):
    codestr = """
    from __static__ import ssize_t, box
    def f():
        x: ssize_t = 1
        y = x
        return box(y)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(), 1)
