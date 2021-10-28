def test_seq_repeat_tuple_reversed(self):
    codestr = """
        def f():
            t = (1, 2)
            return 2 * t
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_TUPLE | SEQ_REPEAT_REVERSED)
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), (1, 2, 1, 2))
