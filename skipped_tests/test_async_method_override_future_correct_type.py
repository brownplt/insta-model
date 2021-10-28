def test_async_method_override_future_correct_type(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 42
            def g(self):
                return self.f()
    """
    with self.in_strict_module(codestr) as mod:
        loop = asyncio.new_event_loop()
        class D(mod.C):
            def f(self):
                fut = loop.create_future()
                fut.set_result(100)
                return fut
        d = D()
        for i in range(100):
            try:
                d.g().send(None)
            except StopIteration as e:
                self.assertEqual(e.args[0], 100)
        loop.close()
