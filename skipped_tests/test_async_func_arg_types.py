def test_async_func_arg_types(self):
    codestr = """
        async def f(x: int):
            pass
    """
    f = self.find_code(self.compile(codestr))
    self.assertInBytecode(f, "CHECK_ARGS", (0, ("builtins", "int")))
