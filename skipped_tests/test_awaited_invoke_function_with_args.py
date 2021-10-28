def test_awaited_invoke_function_with_args(self):
    codestr = """
        async def f(a: int, b: int) -> int:
            return a + b
        async def g() -> int:
            return await f(1, 2)
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.g,
            "INVOKE_FUNCTION",
            ((mod.__name__, "f"), 2),
        )
        self.assertEqual(asyncio.run(mod.g()), 3)
        # exercise shadowcode, INVOKE_FUNCTION_CACHED
        self.make_async_func_hot(mod.g)
        self.assertEqual(asyncio.run(mod.g()), 3)
