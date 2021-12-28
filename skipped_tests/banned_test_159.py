# Reason: Test hitted a banned word f"
def test_cast(self):
    for code_gen in (StaticCodeGenerator, PythonCodeGenerator):
        codestr = """
            from __static__ import cast
            class C:
                pass
            a = C()
            def f() -> C:
                return cast(C, a)
        """
        code = self.compile(codestr, code_gen)
        f = self.find_code(code, "f")
        if code_gen is StaticCodeGenerator:
            self.assertInBytecode(f, "CAST", ("<module>", "C"))
        with self.in_module(codestr, code_gen=code_gen) as mod:
            C = mod.C
            f = mod.f
            self.assertTrue(isinstance(f(), C))
            self.assert_jitted(f)
