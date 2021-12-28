# Reason: Test hitted a banned word int64
def test_inexact_list_large_unsigned(self):
    codestr = """
        from __static__ import uint64
        def f(x: list):
            i: uint64 = 0xffffffffffffffff
            return x[i]
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "type mismatch: uint64 cannot be assigned to dynamic"
    ):
        self.compile(codestr)
