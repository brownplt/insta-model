# Reason: Test hitted a banned word int32
def test_primitive_return_unannotated(self):
    codestr = """
        from __static__ import int32
        def f():
            x: int32 = 1
            return x
    """
    with self.assertRaisesRegex(TypedSyntaxError, bad_ret_type("int32", "dynamic")):
        self.compile(codestr)
