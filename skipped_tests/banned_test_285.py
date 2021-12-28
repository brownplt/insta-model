# Reason: Test hitted a banned word int8
def test_primitive_args_funcdef(self):
    codestr = """
        from __static__ import int8, box
        def n(val: int8):
            return box(val)
        def x():
            y: int8 = 42
            return n(y)
    """
    with self.in_strict_module(codestr) as mod:
        n = mod.n
        x = mod.x
        self.assertEqual(x(), 42)
        self.assertEqual(mod.n(-128), -128)
        self.assertEqual(mod.n(127), 127)
        with self.assertRaises(OverflowError):
            print(mod.n(-129))
        with self.assertRaises(OverflowError):
            print(mod.n(128))
