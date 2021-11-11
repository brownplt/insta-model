# Reason: Test hitted some skipped words
def test_final_decorator_class_dynamic(self):
    """We should never mark DYNAMIC_TYPE as final."""
    codestr = """
        from typing import final, Generic, NamedTuple
        @final
        class NT(NamedTuple):
            x: int
        class C(Generic):
            pass
    """
    # No TypedSyntaxError "cannot inherit from Final class 'dynamic'"
    self.compile(codestr)
