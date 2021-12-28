# Reason: Test hitted a banned word int64
def test_invoke_builtin_func_ret_neg(self):
    # setup xxclassloader as a built-in function for this test, so we can
    # do a direct invoke
    xxclassloader = sys.modules["xxclassloader"]
    try:
        sys.modules["xxclassloader"] = StrictModule(xxclassloader.__dict__, False)
        codestr = """
        from xxclassloader import neg
        from __static__ import int64, box
        def test():
            x: int64 = neg()
            return box(x)
        """
        with self.in_module(codestr) as mod:
            test = mod.test
            self.assertEqual(test(), -1)
    finally:
        sys.modules["xxclassloader"] = xxclassloader
