# Reason: Test hitted a banned word int8
def test_field_size(self):
    for type in [
        "int8",
        "int16",
        "int32",
        "int64",
        "uint8",
        "uint16",
        "uint32",
        "uint64",
    ]:
        codestr = f"""
            from __static__ import {type}, box
            class C{type}:
                def __init__(self):
                    self.a: {type} = 1
                    self.b: {type} = 1
            def testfunc(c: C{type}):
                c.a = 2
                c.b = 3
                return box(c.a + c.b)
            """
        with self.subTest(type=type):
            with self.in_module(codestr) as mod:
                C = getattr(mod, "C" + type)
                f = mod.testfunc
                self.assertEqual(f(C()), 5)
