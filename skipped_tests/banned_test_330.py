# Reason: Test hitted a banned word test_compile_nested_class_in_fn
def test_compile_nested_class_in_fn(self):
    codestr = """
    def fn():
        class C:
            c: int = 1
        return C()
    """
    with self.in_module(codestr) as mod:
        f = mod.fn
        self.assertNotInBytecode(f, "TP_ALLOC")
        self.assertEqual(f().c, 1)
