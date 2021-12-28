# Reason: Test hitted a banned word box
def test_unbox_cbool_typed(self):
    codestr = """
    from __static__ import cbool, box
    def f(i: bool):
        x = cbool(i)
        return box(x)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(True), True)
        self.assertEqual(f(False), False)
        self.assertInBytecode(f, "PRIMITIVE_UNBOX")
        self.assertNotInBytecode(f, "CAST", ("builtins", "bool"))
