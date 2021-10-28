def test_async_method_throw(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 42
            async def g(self):
                coro = self.f()
                return coro.throw(StopIteration(100))
    """
    with self.in_module(codestr) as mod:
        loop = asyncio.new_event_loop()
        class D(mod.C):
            def f(self):
                return loop.create_future()
        coro = D().g()
        try:
            coro.send(None)
        except StopIteration as e:
            self.assertEqual(e.args[0], 100)
        loop.close()
