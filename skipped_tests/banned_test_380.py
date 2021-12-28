# Reason: Test hitted a banned word int64
def test_seq_repeat_primitive_reversed(self):
    codestr = """
        from __static__ import int64
        def f():
            x: int64 = 2
            l = [1, 2]
            return x * l
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(
        f,
        "SEQUENCE_REPEAT",
        SEQ_LIST | SEQ_REPEAT_REVERSED | SEQ_REPEAT_PRIMITIVE_NUM,
    )
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.f(), [1, 2, 1, 2])
