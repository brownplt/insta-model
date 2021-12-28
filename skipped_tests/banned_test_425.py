# Reason: Test hitted a banned word await
def test_awaited_invoke_method_with_args(self):
    codestr = """
        class C:
            async def f(self, a: int, b: int) -> int:
                return a + b
            async def g(self) -> int:
                return await self.f(1, 2)
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.C.g,
            "INVOKE_METHOD",
            ((mod.__name__, "C", "f"), 2),
        )
        self.assertEqual(asyncio.run(mod.C().g()), 3)
        # exercise shadowcode, INVOKE_METHOD_CACHED
        async def make_hot():
            c = mod.C()
            for i in range(50):
                await c.g()
        asyncio.run(make_hot())
        self.assertEqual(asyncio.run(mod.C().g()), 3)
