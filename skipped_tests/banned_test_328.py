# Reason: Test hitted a banned word cbool
def test_compare_with_attr(self):
    codestr = """
    from __static__ import cbool
    class C:
        def __init__(self) -> None:
            self.running: cbool = False
        def f(self) -> int:
            return 2 if not self.running else 1
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        c = C()
        self.assertEqual(c.f(), 2)
