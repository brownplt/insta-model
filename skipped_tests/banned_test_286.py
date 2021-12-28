# Reason: Test hitted a banned word f"
def test_descriptor_access(self):
    value = 42
    expected = value
    codestr = f"""
        class Obj:
            abc: int
        class C:
            x: Obj
        def f():
            return C.x.abc
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "LOAD_ATTR", "abc")
        self.assertNotInBytecode(f, "LOAD_FIELD")
