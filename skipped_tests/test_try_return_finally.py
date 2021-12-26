# Reason: Code hitted some skipped words
def test_try_return_finally(self):
    codestr = """
    from typing import List
    def f1(x: List):
        try:
            return
        finally:
            x.append("hi")
    """
    with self.in_module(codestr) as mod:
        f1 = mod.f1
        l = []
        f1(l)
        self.assertEqual(l, ["hi"])
