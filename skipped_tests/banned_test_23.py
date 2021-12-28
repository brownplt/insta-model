# Reason: Test hitted a banned word int8
def test_store_signed_to_unsigned(self):
    codestr = """
        from __static__ import int8, uint8, uint64, box
        def testfunc(tst=False):
            x: uint8 = 42
            y: int8 = 2
            x = y
    """
    with self.assertRaisesRegex(TypedSyntaxError, type_mismatch("int8", "uint8")):
        self.compile(codestr)
