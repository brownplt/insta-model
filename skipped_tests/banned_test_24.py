# Reason: Test hitted a banned word int8
def test_mixed_tri_add_unsigned(self):
    """promote int/uint to int, can't add to uint64"""
    codestr = """
        from __static__ import int8, uint8, uint64, box
        def testfunc(tst=False):
            x: uint8 = 42
            y: int8 = 2
            z: uint64 = 3
            return box(x + y + z)
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot add int16 and uint64"):
        self.compile(codestr)
