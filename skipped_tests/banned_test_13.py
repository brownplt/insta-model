# Reason: Test hitted a banned word int32
def test_field_unsign_ext(self):
    """tests that we do the correct sign extension when loading from a field"""
    for type, val, test in [("uint32", 65537, -1)]:
        codestr = f"""
            from __static__ import {type}, int64, box
            class C{type}:
                def __init__(self):
                    self.value: {type} = {val}
            def testfunc(c: C{type}):
                z: int64 = {test}
                if c.value < z:
                    return True
                return False
            """
        with self.subTest(type=type, val=val, test=test):
            with self.in_module(codestr) as mod:
                C = getattr(mod, "C" + type)
                f = mod.testfunc
                self.assertEqual(f(C()), False)
