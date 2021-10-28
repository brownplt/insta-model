def test_multi_generic(self):
    codestr = """
    from xxclassloader import XXGeneric
    def func():
        a = XXGeneric[int, str]()
        return a.foo(42, 'abc')
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertEqual(f(), "42abc")
