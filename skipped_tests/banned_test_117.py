# Reason: Test hitted a banned word cbool
def test_await_primitive(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = True
        async def f(abc):
            await cbool(COND)
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot await a primitive value"):
        self.compile(code)
