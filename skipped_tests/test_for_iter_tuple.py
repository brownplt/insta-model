# Reason: Hitted a skipped word (_for_)
def test_for_iter_tuple(self):
    codestr = """
        from typing import List
        def f(n: int) -> List:
            acc = []
            l = tuple([i for i in range(n)])
            for i in l:
                acc.append(i + 1)
            return acc
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "FOR_ITER")
        self.assertEqual(f(4), [i + 1 for i in range(4)])
