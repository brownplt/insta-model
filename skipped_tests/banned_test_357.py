# Reason: Test hitted a banned word await
def test_awaited_invoke_function(self):
    codestr = """
        async def f() -> int:
            return 1
        async def g() -> int:
            return await f()
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(mod.g, "INVOKE_FUNCTION", ((mod.__name__, "f"), 0))
        self.assertNotInBytecode(mod.g, "CAST")
        self.assertEqual(asyncio.run(mod.g()), 1)
