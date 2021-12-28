# Reason: Test hitted a banned word int64
def test_chained_assign_type_propagation(self):
    codestr = """
        from __static__ import int64, char, Array
        def test2() -> Array[char]:
            x = y = Array[char]([48])
            return y
    """
    self.compile(codestr, modname="foo")
