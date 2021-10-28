def test_invoke_function(self):
    my_int = "12345"
    codestr = f"""
    def x(a: str, b: int) -> str:
        return a + str(b)
    def test() -> str:
        return x("hello", {my_int})
    """
    c = self.compile(codestr, modname="foo.py")
    test = self.find_code(c, "test")
    self.assertInBytecode(test, "INVOKE_FUNCTION", (("foo.py", "x"), 2))
    with self.in_module(codestr) as mod:
        test_callable = mod.test
        self.assertEqual(test_callable(), "hello" + my_int)
