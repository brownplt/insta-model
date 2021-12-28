# Reason: Test hitted a banned word cbool
def test_generator_primitive_element(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = True
        def f(abc):
            return [cbool(COND) for x in abc]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "generator element cannot be a primitive"
    ):
        self.compile(code)
