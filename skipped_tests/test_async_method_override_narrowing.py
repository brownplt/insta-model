def test_async_method_override_narrowing(self):
    codestr = """
        class Num(int):
            pass
        class C:
            async def f(self) -> int:
                return 0
        class D(C):
            async def f(self) -> Num:
                return Num(0)
    """
    with self.in_strict_module(codestr) as mod:
        d = mod.D()
        try:
            d.f().send(None)
        except StopIteration as e:
            res = e.args[0]
            self.assertIsInstance(res, mod.Num)
            self.assertEqual(res, 0)
