def test_async_method_override_future_incorrect_type(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 42
            def g(self):
                return self.f()
    """
    with self.in_module(codestr) as mod:
        loop = asyncio.new_event_loop()
        class D(mod.C):
            def f(self):
                fut = loop.create_future()
                fut.set_result("not an int")
                return fut
        d = D()
        with self.assertRaises(TypeError):
            d.g().send(None)
        loop.close()
