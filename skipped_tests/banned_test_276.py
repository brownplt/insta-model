# Reason: Test hitted a banned word await
def test_async_method_immediate_await_incorrect_type(self):
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
                return "not an int"
        d = D()
        with self.assertRaises(TypeError):
            asyncio.run(mod.f(d))
