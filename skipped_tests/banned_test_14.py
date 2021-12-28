# Reason: Test hitted a banned word int32
def test_field_sign_compare(self):
    for type, val, test in [("int32", -1, -1)]:
        codestr = f"""
            from __static__ import {type}, box
            class C{type}:
                def __init__(self):
                    self.value: {type} = {val}
            def testfunc(c: C{type}):
                if c.value == {test}:
                    return True
                return False
            """
        with self.subTest(type=type, val=val, test=test):
            with self.in_module(codestr) as mod:
                C = getattr(mod, "C" + type)
                f = mod.testfunc
                self.assertTrue(f(C()))
