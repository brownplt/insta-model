# Reason: Test hitted a banned word int8
def test_primitive_args_funcdef_missing_starargs(self):
    codestr = """
        from __static__ import int8, box
        def x(val: int8, *foo):
            return box(val), foo
        def y(val: int8, **foo):
            return box(val), foo
    """
    with self.in_strict_module(codestr) as mod:
        self.assertEqual(mod.x(-128), (-128, ()))
        self.assertEqual(mod.y(-128), (-128, {}))
