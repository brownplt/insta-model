# Reason: Test hitted a banned word cbool
def test_yield_primitive(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = True
        def f(abc):
            yield cbool(COND)
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot yield a primitive value"):
        self.compile(code)
