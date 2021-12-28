# Reason: Can't be translated by any of the three translator
def test_seq_repeat_list(self):
    codestr = """
        def f():
            l = [1, 2]
            return l * 2
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_LIST)
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), [1, 2, 1, 2])
