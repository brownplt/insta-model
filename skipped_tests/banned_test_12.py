# Reason: Test hitted a banned word int8
def test_field_sign_ext(self):
    """tests that we do the correct sign extension when loading from a field"""
    for type, val in [
        ("int32", 65537),
        ("int16", 256),
        ("int8", 0x7F),
        ("uint32", 65537),
    ]:
        codestr = f"""
            from __static__ import {type}, box
            class C{type}:
                def __init__(self):
                    self.value: {type} = {val}
            def testfunc(c: C{type}):
                return box(c.value)
            """
        with self.subTest(type=type, val=val):
            with self.in_module(codestr) as mod:
                C = getattr(mod, "C" + type)
                f = mod.testfunc
                self.assertEqual(f(C()), val)
