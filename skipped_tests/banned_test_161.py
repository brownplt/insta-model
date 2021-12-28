# Reason: Test hitted a banned word f"
def test_cast_optional(self):
    for code_gen in (StaticCodeGenerator, PythonCodeGenerator):
        codestr = """
            from __static__ import cast
            from typing import Optional
            class C:
                pass
            def f(x) -> Optional[C]:
                return cast(Optional[C], x)
        """
        code = self.compile(codestr, code_gen)
        f = self.find_code(code, "f")
        if code_gen is StaticCodeGenerator:
            self.assertInBytecode(f, "CAST", ("<module>", "C", "?"))
        with self.in_module(codestr, code_gen=code_gen) as mod:
            C = mod.C
            f = mod.f
            self.assertTrue(isinstance(f(C()), C))
            self.assertEqual(f(None), None)
            self.assert_jitted(f)
