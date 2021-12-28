# Reason: Test hitted a banned word ...
def test_typing_overload_toplevel(self) -> None:
    """Typing overloads are ignored, don't cause member name conflict."""
    codestr = """
        from typing import Optional, overload
        @overload
        def bar(x: int) -> int:
            ...
        def bar(x: Optional[int]) -> Optional[int]:
            return x
        def f(x: int) -> Optional[int]:
            return bar(x)
    """
    self.assertReturns(codestr, "Optional[int]")
