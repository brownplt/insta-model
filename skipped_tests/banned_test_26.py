# Reason: Test hitted a banned word int8
def test_store_unsigned_to_signed(self):
    """promote int/uint to int, can't add to uint64"""
    codestr = """
        from __static__ import int8, uint8, uint64, box
        def testfunc(tst=False):
            x: uint8 = 42
            y: int8 = 2
            y = x
    """
    with self.assertRaisesRegex(TypedSyntaxError, type_mismatch("uint8", "int8")):
        self.compile(codestr)
