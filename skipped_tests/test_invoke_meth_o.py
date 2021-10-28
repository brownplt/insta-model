def test_invoke_meth_o(self):
    codestr = """
    from xxclassloader import spamobj
    def func():
        a = spamobj[int]()
        a.setstate_untyped(42)
        return a.getstate()
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertInBytecode(
            f,
            "INVOKE_METHOD",
            (
                (
                    "xxclassloader",
                    "spamobj",
                    (("builtins", "int"),),
                    "setstate_untyped",
                ),
                1,
            ),
        )
        self.assertEqual(f(), 42)
        self.assert_jitted(f)
