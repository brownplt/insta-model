# Reason: Test hitted a banned word int64
def test_vector_nogc(self):
    codestr = """
        from __static__ import Vector, int64
        class C:
            foo: Vector[int64]
            def __init__(self):
                self.foo = Vector[int64]()
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        x = C()
        self.assertFalse(gc.is_tracked(x))
