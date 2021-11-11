# Reason: Test hitted some skipped words
def test_for_iter_unchecked_get(self):
    """We don't need to check sequence bounds when we've just compared with the list size."""
    codestr = """
        def f():
            l = [1, 2, 3]
            acc = []
            for x in l:
                acc.append(x)
            return acc
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "SEQUENCE_GET", SEQ_LIST | SEQ_SUBSCR_UNCHECKED)
        self.assertEqual(f(), [1, 2, 3])
