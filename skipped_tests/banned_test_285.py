# Reason: Test hitted a banned word f"
def test_class_unknown_attr(self):
    value = 42
    expected = value
    codestr = f"""
        class C:
            pass
        def f():
            return C.foo
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "LOAD_ATTR", "foo")
