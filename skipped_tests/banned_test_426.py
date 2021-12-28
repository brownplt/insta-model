# Reason: Test hitted a banned word await
def test_awaited_invoke_method_future(self):
    codestr = """
        from asyncio import ensure_future
        async def h() -> int:
            return 1
        class C:
            async def g(self) -> None:
                await ensure_future(h())
        async def f():
            c = C()
            await c.g()
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.f,
            "INVOKE_METHOD",
            ((mod.__name__, "C", "g"), 0),
        )
        asyncio.run(mod.f())
        # exercise shadowcode, INVOKE_METHOD_CACHED
        self.make_async_func_hot(mod.f)
        asyncio.run(mod.f())
