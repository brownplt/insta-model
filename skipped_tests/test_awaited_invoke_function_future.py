def test_awaited_invoke_function_future(self):
    codestr = """
        from asyncio import ensure_future
        async def h() -> int:
            return 1
        async def g() -> None:
            await ensure_future(h())
        async def f():
            await g()
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.f,
            "INVOKE_FUNCTION",
            ((mod.__name__, "g"), 0),
        )
        asyncio.run(mod.f())
        # exercise shadowcode
        self.make_async_func_hot(mod.f)
        asyncio.run(mod.f())
