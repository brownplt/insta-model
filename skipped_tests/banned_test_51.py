# Reason: Test hitted a banned word int32
def test_disallow_prim_nonprim_union(self):
    codestr = """
        from __static__ import int32
        def f(y: int):
            x: int32 = 2
            z = x or y
            return z
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"invalid union type Union\[int32, int\]; unions cannot include primitive types",
    ):
        self.compile(codestr)
