# Reason: Test hitted a banned word int8
def test_rand_max_inlined2(self):
    codestr = """
        from __static__ import rand, RAND_MAX, box, int8, int64
        def f() -> int:
            x: int64 = rand() // int8(RAND_MAX)
            return box(x)
    """
    self.type_error(
        codestr,
        re.escape("type mismatch: Literal[2147483647] cannot be assigned to int8"),
    )
