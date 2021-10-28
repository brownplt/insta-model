def test_async_method_close(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 42
            async def g(self):
                coro = self.f()
                return coro.close()
    """
    with self.in_module(codestr) as mod:
        class D(mod.C):
            async def f(self):
                return 0
        coro = D().g()
        try:
            coro.send(None)
        except StopIteration as e:
            self.assertEqual(e.args, ())
