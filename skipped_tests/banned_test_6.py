# Reason: Test hitted a banned word nonlocal
def test_deep_attr_chain(self):
    """this shouldn't explode exponentially"""
    codestr = """
    def f(x):
        return x.x.x.x.x.x.x
    """
    class C:
        def __init__(self):
            self.x = self
    orig_bind_attr = Object.bind_attr
    call_count = 0
    def bind_attr(*args):
        nonlocal call_count
        call_count += 1
        return orig_bind_attr(*args)
    with patch("compiler.static.types.Object.bind_attr", bind_attr):
        with self.in_module(codestr) as mod:
            f = mod.f
            x = C()
            self.assertEqual(f(x), x)
            # Initially this would be 63 when we were double visiting
            self.assertLess(call_count, 10)
