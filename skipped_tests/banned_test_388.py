# Reason: Test hitted a banned word f"
def test_chained_compare(self):
    for jumpif in [False, True]:
        with self.subTest(jumpif=jumpif):
            if jumpif:
                pre = ""
                test = "0 < x < 10"
            else:
                pre = "y = 0 < x < 10"
                test = "y"
            codestr = f"""
                def f(x):
                    {pre}
                    if {test}:
                        return 1
                    return 0
            """
            with self.in_module(codestr) as mod:
                f = mod.f
                self.assertEqual(f(0), 0)
                self.assertEqual(f(1), 1)
                self.assertEqual(f(9), 1)
                self.assertEqual(f(10), 0)
