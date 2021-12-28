# Reason: Test hitted a banned word async
def test_async_func_ret_type(self):
    codestr = """
        async def x(a) -> int:
            return a
    """
    f = self.find_code(self.compile(codestr, modname="foo"))
    self.assertInBytecode(f, "CAST")
