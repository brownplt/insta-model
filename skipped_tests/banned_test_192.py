# Reason: Test hitted a banned word global
def test_starargs_invoked_in_order(self):
    codestr = """
        X = 1
        def f():
            global X
            X += 1
            return {"a": 1, "b": "foo"}
        def make_c():
            global X
            X *= 2
            return 42
        class C:
            def x(self, a: int=1, b: str="hunter2", c: int=14) -> None:
                return
        def test():
            C().x(12, c=make_c(), **f())
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        test()
        x = mod.X
        self.assertEqual(x, 3)
