def test_async_method_override(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 1
        def f(x: C):
            return x.f()
    """
    with self.in_strict_module(codestr) as mod:
        class D(mod.C):
            async def f(self):
                return "not an int"
        self.assertInBytecode(
            mod.f,
            "INVOKE_METHOD",
            ((mod.__name__, "C", "f"), 0),
        )
        d = D()
        with self.assertRaises(TypeError):
            asyncio.run(mod.f(d))
