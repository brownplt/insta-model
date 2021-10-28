def test_compile_nested_class_in_fn(self):
    codestr = """
    def fn():
        class C:
            c: int = 1
        return C()
    """
    with self.in_module(codestr) as mod:
        f = mod.fn
        self.assertInBytecode(f, "CALL_FUNCTION")
