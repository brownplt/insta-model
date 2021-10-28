def test_async_method_incorrect_type(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 1
        async def f(x: C):
            a = x.f()
            b = 2
            c = await a
            return b + c
    """
    with self.in_strict_module(codestr) as mod:
        class D(mod.C):
            async def f(self):
                return "not an int"
        d = D()
        with self.assertRaises(TypeError):
            asyncio.run(mod.f(d))
