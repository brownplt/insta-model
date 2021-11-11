# Reason: Test hitted some skipped words
def test_cast_fail(self):
    for code_gen in (StaticCodeGenerator, PythonCodeGenerator):
        codestr = """
            from __static__ import cast
            class C:
                pass
            def f() -> C:
                return cast(C, 42)
        """
        code = self.compile(codestr, code_gen)
        f = self.find_code(code, "f")
        if code_gen is StaticCodeGenerator:
            self.assertInBytecode(f, "CAST", ("<module>", "C"))
        with self.in_module(codestr) as mod:
            with self.assertRaises(TypeError):
                f = mod.f
                f()
                self.assert_jitted(f)
