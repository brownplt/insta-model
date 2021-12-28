# Reason: Test hitted a banned word int8
def test_primitive_args_funcdef_too_many_args(self):
    codestr = """
        from __static__ import int8, box
        def n(x: int8):
            return box(x)
    """
    with self.in_strict_module(codestr) as mod:
        n = mod.n
        with self.assertRaises(TypeError):
            print(mod.n(-128, x=2))
        with self.assertRaises(TypeError):
            print(mod.n(-128, 2))
