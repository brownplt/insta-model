# Reason: Test hitted some skipped words
def test_for_iter_list_modified(self):
    codestr = """
        def f():
            l = [1, 2, 3, 4, 5]
            acc = []
            for x in l:
                acc.append(x)
                l[2:] = []
            return acc
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "FOR_ITER")
        self.assertEqual(f(), [1, 2])
