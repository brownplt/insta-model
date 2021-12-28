# Reason: Test hitted a banned word cbool
def test_dict_comp_primitive_element(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = True
        def f(abc):
            return {k:cbool(COND) for k in abc}
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "dictionary comprehension value cannot be a primitive"
    ):
        self.compile(code)
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = True
        def f(abc):
            return {cbool(COND):v for v in abc}
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "dictionary comprehension key cannot be a primitive"
    ):
        self.compile(code)
