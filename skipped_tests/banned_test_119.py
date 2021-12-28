# Reason: Test hitted a banned word cbool
def test_yield_from_primitive(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = True
        def f(abc):
            yield from cbool(COND)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot yield from a primitive value"
    ):
        self.compile(code)
