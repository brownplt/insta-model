def test_async_method_throw_incorrect_type(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 42
            async def g(self):
                coro = self.f()
                return coro.throw(StopIteration("not an int"))
    """
    with self.in_module(codestr) as mod:
        loop = asyncio.new_event_loop()
        class D(mod.C):
            def f(self):
                return loop.create_future()
        coro = D().g()
        with self.assertRaises(TypeError):
            coro.send(None)
        loop.close()
