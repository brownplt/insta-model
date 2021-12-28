# Reason: Test hitted a banned word async
def test_async_method_override_widening(self):
    codestr = """
        from typing import Optional
        class C:
            async def f(self) -> int:
                return 0
        class D(C):
            async def f(self) -> Optional[int]:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Returned type `static.InferredAwaitable\[Optional\[int\]\]` is not "
        r"a subtype of the overridden return `static.InferredAwaitable\[int\]`",
    ):
        self.compile(codestr, modname="foo")
