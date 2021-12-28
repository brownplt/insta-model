# Reason: Test hitted a banned word nonlocal
def test_field_refcount(self):
    codestr = """
        class C:
            def __init__(self):
                self.x = None
            def set_x(self, x):
                self.x = x
    """
    count = 0
    with self.in_module(codestr) as mod:
        C = mod.C
        class X:
            def __init__(self):
                nonlocal count
                count += 1
            def __del__(self):
                nonlocal count
                count -= 1
        c = C()
        c.set_x(X())
        c.set_x(X())
        self.assertEqual(count, 1)
        del c
        self.assertEqual(count, 0)
