# Reason: Format too complicated
def test_sorted(self):
    """sorted() builtin returns an Exact[List]."""
    codestr = """
        from typing import Iterable
        def f(l: Iterable[int]):
            for x in sorted(l):
                pass
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "FOR_ITER")
        self.assertInBytecode(f, "REFINE_TYPE", ("builtins", "list"))
