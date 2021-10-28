def test_final_decorator_override_transitivity(self):
    codestr = """
    from typing import final
    class C:
        @final
        def f():
            pass
    class D(C):
        pass
    class E(D):
        def f():
            pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final attribute of foo.E:f"
    ):
        self.compile(codestr, modname="foo")
