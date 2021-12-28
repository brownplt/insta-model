# Reason: Test hitted a banned word int32
def test_array_call_typecheck(self):
    codestr = """
        from __static__ import Array, int32
        def h(x: Array[int32]) -> int32:
            return x[0]
    """
    error_msg = re.escape("h expected 'Array[int32]' for argument x, got 'list'")
    with self.in_module(codestr) as mod:
        with self.assertRaisesRegex(TypeError, error_msg):
            mod.h(["B"])
