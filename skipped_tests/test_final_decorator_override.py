def test_final_decorator_override(self):
    codestr = """
    from typing import final
    class C:
        @final
        def f():
            pass
    class D(C):
        def f():
            pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final attribute of foo.D:f"
    ):
        self.compile(codestr, modname="foo")
