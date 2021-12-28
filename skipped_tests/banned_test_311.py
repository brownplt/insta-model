# Reason: Test hitted a banned word int8
def test_assign_bool_to_primitive_int(self):
    codestr = f"""
    from __static__ import int8
    def f() -> int:
        a: int8 = True
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[bool]", "int8")
    ):
        self.compile(codestr)
