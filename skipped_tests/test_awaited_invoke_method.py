def test_awaited_invoke_method(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 1
            async def g(self) -> int:
                return await self.f()
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.C.g, "INVOKE_METHOD", ((mod.__name__, "C", "f"), 0)
        )
        self.assertEqual(asyncio.run(mod.C().g()), 1)
