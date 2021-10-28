def test_nested_for_iter_sequence_break(self):
    codestr = """
        from typing import List
        def f(n: int) -> List:
            acc = []
            l = [i for i in range(n)]
            for i in l:
                for j in l:
                    if j == 2:
                        break
                    acc.append(i + j)
            return acc
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "FOR_ITER")
        self.assertEqual(f(3), [0, 1, 1, 2, 2, 3])
