# Reason: Test hitted a banned word int32
def test_primitive_compare_immediate_no_branch_on_result(self):
    for rev in [True, False]:
        compare = "0 == xp" if rev else "xp == 0"
        codestr = f"""
            from __static__ import box, int64, int32
            def f(x: int) -> bool:
                xp = int64(x)
                y = {compare}
                return box(y)
        """
        with self.subTest(rev=rev):
            with self.in_module(codestr) as mod:
                f = mod.f
                self.assertEqual(f(3), 0)
                self.assertEqual(f(0), 1)
                self.assertIs(f(0), True)
