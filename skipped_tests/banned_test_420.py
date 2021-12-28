# Reason: Test hitted a banned word await
def test_awaited_invoke_function_unjitable(self):
    codestr = """
        async def f() -> int:
            class C: pass
            return 1
        async def g() -> int:
            return await f()
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.g,
            "INVOKE_FUNCTION",
            ((mod.__name__, "f"), 0),
        )
        self.assertEqual(asyncio.run(mod.g()), 1)
        self.assert_not_jitted(mod.f)
