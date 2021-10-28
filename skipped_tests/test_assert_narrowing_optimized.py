def test_assert_narrowing_optimized(self):
    # We ensure that the code without the assert would work in the runtime.
    codestr = """
    def foo(x: int | str) -> object:
        assert isinstance(x, int)
        return x
    """
    with self.in_module(codestr, optimize=1) as mod:
        foo = mod.foo
        self.assertEqual(foo(1), 1)
        with self.assertRaises(TypeError):
            foo("a")
