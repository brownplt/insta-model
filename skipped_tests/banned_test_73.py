# Reason: Test hitted a banned word box
def test_box_cbool_to_bool(self):
    codestr = """
        from typing import final
        from __static__ import cbool
        def foo() -> bool:
            b: cbool = True
            return bool(b)
    """
    with self.in_module(codestr) as mod:
        self.assertInBytecode(mod.foo, "PRIMITIVE_BOX")
        self.assertTrue(mod.foo())
