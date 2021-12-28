# Reason: Test hitted a banned word async
def test_async_method_throw_exception(self):
    codestr = """
        class C:
            async def f(self) -> int:
                return 42
            async def g(self):
                coro = self.f()
                return coro.throw(IndexError("ERROR"))
    """
    with self.in_module(codestr) as mod:
        class D(mod.C):
            async def f(self):
                return 0
        coro = D().g()
        with self.assertRaises(IndexError):
            coro.send(None)
