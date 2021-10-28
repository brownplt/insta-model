def test_async_method_immediate_await(self):
    codestr = """
        class C:
            async def f(self) -> bool:
                return True
        async def f(x: C):
            if await x.f():
                return 0
            return 1
    """
    with self.in_strict_module(codestr) as mod:
        class D(mod.C):
            async def f(self):
                return False
        d = D()
        self.assertEqual(asyncio.run(mod.f(d)), 1)
