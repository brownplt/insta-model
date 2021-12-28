# Reason: Test hitted a banned word cbool
def test_generator_primitive_iter(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = False
        def f(abc):
            return [x for x in cbool(COND)]
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot iterate over cbool"):
        self.compile(code)
