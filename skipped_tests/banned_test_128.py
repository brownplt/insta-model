# Reason: Test hitted a banned word b"
def test_cross_module_inheritance(self) -> None:
    acode = """
        class C:
            def f(self):
                return 42
    """
    bcode = """
        from a import C
        class D(C):
            def f(self):
                return 'abc'
        def f(y):
            x: C
            if y:
                x = D()
            else:
                x = C()
            return x.f()
    """
    bcomp = self.compiler(a=acode, b=bcode).compile_module("b")
    x = self.find_code(bcomp, "f")
    self.assertInBytecode(x, "INVOKE_METHOD", (("a", "C", "f"), 0))
