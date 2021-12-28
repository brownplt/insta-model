# Reason: Test hitted a banned word int8
def test_augassign_primitive_int(self):
    codestr = """
    from __static__ import int8, box, unbox
    def a(i: int) -> int:
        j: int8 = unbox(i)
        j += 2
        return box(j)
    """
    with self.in_module(codestr) as mod:
        a = mod.a
        self.assertInBytecode(a, "PRIMITIVE_BINARY_OP", 0)
        self.assertEqual(a(3), 5)
