def test_dynamic_base(self):
    nonstatic_code = """
        class Foo:
            pass
    """
    with self.in_module(
        nonstatic_code, code_gen=PythonCodeGenerator, name="nonstatic"
    ):
        codestr = """
            from nonstatic import Foo
            class A(Foo):
                def __init__(self):
                    self.x = 1
                def f(self) -> int:
                    return self.x
            def f(x: A) -> int:
                return x.f()
        """
        with self.in_module(codestr) as mod:
            f = mod.f
            self.assertInBytecode(f, "INVOKE_METHOD")
            a = mod.A()
            self.assertEqual(f(a), 1)
            # x is a data descriptor, it takes precedence
            a.__dict__["x"] = 100
            self.assertEqual(f(a), 1)
            # but methods are normal descriptors, instance
            # attributes should take precedence
            a.__dict__["f"] = lambda: 42
            self.assertEqual(f(a), 42)
