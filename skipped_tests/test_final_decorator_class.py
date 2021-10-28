def test_final_decorator_class(self):
    codestr = """
    from typing import final
    @final
    class C:
        def f(self):
            pass
    def f():
        return C().f()
    """
    c = self.compile(codestr, modname="foo")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "INVOKE_FUNCTION")
