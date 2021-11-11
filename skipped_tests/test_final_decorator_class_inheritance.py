# Reason: Test hitted some skipped words
def test_final_decorator_class_inheritance(self):
    codestr = """
    from typing import final
    @final
    class C:
        pass
    class D(C):
        pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Class `foo.D` cannot subclass a Final class: `foo.C`"
    ):
        self.compile(codestr, modname="foo")
