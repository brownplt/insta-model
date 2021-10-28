def test_awaited_invoke_function_indirect_with_args(self):
    codestr = """
        async def f(a: int, b: int) -> int:
            return a + b
        async def g() -> int:
            return await f(1, 2)
    """
    with self.in_module(codestr) as mod:
        g = mod.g
        self.assertInBytecode(
            g,
            "INVOKE_FUNCTION",
            ((mod.__name__, "f"), 2),
        )
        self.assertEqual(asyncio.run(g()), 3)
        # exercise shadowcode, INVOKE_FUNCTION_INDIRECT_CACHED
        self.make_async_func_hot(g)
        self.assertEqual(asyncio.run(g()), 3)
