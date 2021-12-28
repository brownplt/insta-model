# Reason: Test hitted a banned word cbool
def test_cbool_field(self):
    codestr = """
        from __static__ import cbool
        class C:
            def __init__(self, x: cbool) -> None:
                self.x: cbool = x
        def f(c: C):
            if c.x:
                return True
            return False
    """
    with self.in_module(codestr) as mod:
        f, C = mod.f, mod.C
        self.assertInBytecode(f, "LOAD_FIELD", (mod.__name__, "C", "x"))
        self.assertInBytecode(f, "POP_JUMP_IF_ZERO")
        self.assertIs(C(True).x, True)
        self.assertIs(C(False).x, False)
        self.assertIs(f(C(True)), True)
        self.assertIs(f(C(False)), False)
