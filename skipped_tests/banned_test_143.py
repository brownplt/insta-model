# Reason: Test hitted a banned word int64
def test_invoke_builtin_func_arg(self):
    codestr = """
    from xxclassloader import bar
    from __static__ import int64, box
    def func():
        a: int64 = bar(42)
        return box(a)
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertInBytecode(f, "INVOKE_FUNCTION", (((mod.__name__, "bar"), 1)))
        self.assertEqual(f(), 42)
        self.assert_jitted(f)
