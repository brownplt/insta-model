def test_for_iter_sequence_orelse_break(self):
    codestr = """
        from typing import List
        def f(n: int) -> List:
            acc = []
            l = [i for i in range(n)]
            for i in l:
                if i == 2:
                    break
                acc.append(i + 1)
            else:
                acc.append(999)
            return acc
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "FOR_ITER")
        self.assertEqual(f(4), [1, 2])
