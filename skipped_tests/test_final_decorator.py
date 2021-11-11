# Reason: Test hitted some skipped words
def test_final_decorator(self):
    codestr = """
    from typing import final
    class C:
        @final
        def f():
            pass
    """
    self.compile(codestr, modname="foo")
