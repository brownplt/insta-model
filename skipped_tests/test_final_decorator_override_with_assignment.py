# Reason: Test hitted some skipped words
def test_final_decorator_override_with_assignment(self):
    codestr = """
    from typing import final
    class C:
        @final
        def f():
            pass
    class D(C):
        f = print
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final attribute of foo.D:f"
    ):
        self.compile(codestr, modname="foo")
