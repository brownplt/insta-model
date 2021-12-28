# Reason: Test hitted a banned word cbool
def test_generator_primitive_condition(self):
    code = """
        from __static__ import cbool
        from typing import Final
        COND: Final[bool] = False
        def f(abc):
            return [x for x in abc if cbool(COND)]
    """
    with self.in_module(code) as mod:
        f = mod.f
        self.assertInBytecode(f.__code__, "POP_JUMP_IF_ZERO")
        self.assertEqual(f([1, 2, 3]), [])
