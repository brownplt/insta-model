# Reason: Test hitted a banned word box
def test_unbox_cbool(self):
    codestr = """
    from __static__ import cbool, box
    def f(i: object):
        x = cbool(i)
        return box(x)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "PRIMITIVE_UNBOX")
        self.assertInBytecode(f, "CAST", ("builtins", "bool"))
        self.assertEqual(f(True), True)
        self.assertEqual(f(False), False)
        with self.assertRaises(TypeError):
            self.assertEqual(f(42), True)
