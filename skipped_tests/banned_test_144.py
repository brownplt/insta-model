# Reason: Test hitted a banned word int64
def test_invoke_func_unexistent_module(self):
    codestr = """
    from xxclassloader import bar
    from __static__ import int64, box
    def func():
        a: int64 = bar(42)
        return box(a)
    """
    with self.in_module(codestr) as mod:
        # remove xxclassloader from sys.modules during this test
        xxclassloader = sys.modules["xxclassloader"]
        del sys.modules["xxclassloader"]
        try:
            func = mod.func
            self.assertInBytecode(
                func, "INVOKE_FUNCTION", (((mod.__name__, "bar"), 1))
            )
            self.assertEqual(func(), 42)
            self.assert_jitted(func)
        finally:
            sys.modules["xxclassloader"] = xxclassloader
