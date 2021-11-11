# Reason: Test hitted some skipped words
def test_invoke_strict_module_deep_unjitable(self):
    codestr = """
        def f12(): return 42
        def f11():
            class C: pass
            return f12()
        def f10(): return f11()
        def f9(): return f10()
        def f8(): return f9()
        def f7(): return f8()
        def f6(): return f7()
        def f5(): return f6()
        def f4(): return f5()
        def f3(): return f4()
        def f2(): return f3()
        def f1(): return f2()
        def g(x):
            if x: return 0
            return f1()
    """
    with self.in_strict_module(codestr) as mod:
        g = mod.g
        self.assertEqual(g(True), 0)
        # we should have done some level of pre-jitting
        self.assert_not_jitted(mod.f10)
        self.assert_not_jitted(mod.f11)
        self.assert_not_jitted(mod.f12)
        [self.assert_jitted(getattr(mod, f"f{i}")) for i in range(1, 10)]
        self.assertEqual(g(False), 42)
        self.assertInBytecode(
            g,
            "INVOKE_FUNCTION",
            ((mod.__name__, "f1"), 0),
        )
