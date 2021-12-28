# Reason: Test hitted a banned word cbool
def test_cbool_compare_to_cbool(self):
    for a, b in [
        ("True", "True"),
        ("True", "False"),
        ("False", "False"),
        ("False", "True"),
    ]:
        codestr = f"""
        from __static__ import cbool
        def f() -> int:
            a: cbool = {a}
            b: cbool = {b}
            if a < b:
                return 1
            else:
                return 2
        """
        with self.subTest(a=a, b=b):
            with self.in_module(codestr) as mod:
                f = mod.f
                if a == "True":
                    self.assertEqual(f(), 2)
                elif a == "False" and b == "False":
                    self.assertEqual(f(), 2)
                else:
                    self.assertEqual(f(), 1)
