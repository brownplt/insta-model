# Reason: Test hitted a banned word ...
def test_typing_overload(self) -> None:
    """Typing overloads are ignored, don't cause member name conflict."""
    codestr = """
        from typing import Optional, overload
        class C:
            @overload
            def foo(self, x: int) -> int:
                ...
            def foo(self, x: Optional[int]) -> Optional[int]:
                return x
        def f(x: int) -> Optional[int]:
            return C().foo(x)
    """
    self.assertReturns(codestr, "Optional[int]")
