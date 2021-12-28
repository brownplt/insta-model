# Reason: Test hitted a banned word cbool
def test_cbool(self):
    for b in ("True", "False"):
        codestr = f"""
        from __static__ import cbool
        def f() -> int:
            x: cbool = {b}
            if x:
                return 1
            else:
                return 2
        """
        with self.subTest(b=b):
            with self.in_module(codestr) as mod:
                f = mod.f
                self.assertInBytecode(f, "PRIMITIVE_LOAD_CONST")
                self.assertInBytecode(
                    f, "STORE_LOCAL", (0, ("__static__", "cbool"))
                )
                self.assertInBytecode(f, "POP_JUMP_IF_ZERO")
                self.assertEqual(f(), 1 if b == "True" else 2)
