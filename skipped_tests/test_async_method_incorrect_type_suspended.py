def test_async_method_incorrect_type_suspended(self):
    codestr = """
        import asyncio
        class C:
            async def f(self) -> int:
                return 1
        async def f(x: C):
            return await x.f()
    """
    with self.in_strict_module(codestr) as mod:
        class D(mod.C):
            async def f(self):
                await asyncio.sleep(0)
                return "not an int"
        d = D()
        with self.assertRaises(TypeError):
            asyncio.run(mod.f(d))
