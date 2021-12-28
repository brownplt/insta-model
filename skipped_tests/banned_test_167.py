# Reason: Test hitted a banned word global
def test_starargs_invoked_once(self):
    codestr = """
        X = 0
        def f():
            global X
            X += 1
            return {"a": 1, "b": "foo", "c": 42}
        class C:
            def x(self, a: int=1, b: str="hunter2", c: int=14) -> None:
                return
        C().x(12, **f())
    """
    with self.in_module(codestr) as mod:
        x = mod.X
        self.assertEqual(x, 1)
    compiled = self.compile(codestr)
    self.assertEqual(compiled.co_nlocals, 1)
